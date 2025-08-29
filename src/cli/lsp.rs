use crate::common::Position;
use crate::filesystem::files::{self as files, ProjectRoot};
use crate::hop::runtime::HopMode;
use crate::hop::server::{DefinitionLocation, HoverInfo, RenameLocation, Server};
use std::path::Path;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server as LspServer};

pub struct HopLanguageServer {
    client: Client,
    server: RwLock<Server>,
}

impl HopLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            server: RwLock::new(Server::new(HopMode::Dev)),
        }
    }

    fn from_lsp_position(position: tower_lsp::lsp_types::Position) -> Position {
        Position::new(position.line as usize + 1, position.character as usize + 1)
    }

    // Convert from Rust position (1-based) to LSP position (0-based)
    fn to_lsp_position(position: Position) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position {
            line: (position.line - 1) as u32,
            character: (position.column - 1) as u32,
        }
    }

    // Convert from common Range to LSP Range
    fn to_lsp_range(range: crate::common::Range) -> tower_lsp::lsp_types::Range {
        tower_lsp::lsp_types::Range {
            start: Self::to_lsp_position(range.start),
            end: Self::to_lsp_position(range.end),
        }
    }

    fn find_root(uri: &Url) -> anyhow::Result<ProjectRoot> {
        ProjectRoot::find_upwards(std::path::Path::new(uri.path()))
    }

    fn uri_to_module_name(uri: &Url, root: &ProjectRoot) -> String {
        match files::path_to_module_name(Path::new(uri.path()), root) {
            Ok(s) => s,
            Err(_) => panic!(),
        }
    }

    fn module_name_to_uri(name: &str, root: &ProjectRoot) -> Url {
        let p = files::module_name_to_path(name, root);
        match Url::from_file_path(&p) {
            Ok(url) => url,
            Err(_) => panic!(),
        }
    }

    fn convert_hover(&self, hover_info: HoverInfo) -> Hover {
        Hover {
            contents: HoverContents::Scalar(MarkedString::String(hover_info.type_str)),
            range: Some(Self::to_lsp_range(hover_info.range)),
        }
    }

    async fn load_all_modules_from_fs(&self, root: &ProjectRoot) -> std::io::Result<()> {
        let all_modules = files::load_all_hop_modules(root)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        for (module_name, content) in all_modules {
            {
                let server = self.server.read().await;
                if server.has_module(&module_name) {
                    continue;
                }
            }
            let mut server = self.server.write().await;
            server.update_module(module_name, &content);
        }

        Ok(())
    }

    async fn publish_diagnostics(&self, root: &ProjectRoot, uri: &Url) {
        let module_name = Self::uri_to_module_name(uri, root);
        let server = self.server.read().await;
        let diagnostics = server.get_error_diagnostics(&module_name);

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = diagnostics
            .into_iter()
            .map(|d| tower_lsp::lsp_types::Diagnostic {
                range: Self::to_lsp_range(d.range),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("hop".to_string()),
                message: d.message,
                related_information: None,
                tags: None,
                data: None,
            })
            .collect();

        self.client
            .publish_diagnostics(uri.clone(), lsp_diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for HopLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "hop-language-server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let root = Self::find_root(&uri).unwrap();
        let _ = self.load_all_modules_from_fs(&root).await;
        self.publish_diagnostics(&root, &uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let root = Self::find_root(&uri).unwrap();
        let module_name = Self::uri_to_module_name(&uri, &root);
        if let Some(change) = params.content_changes.into_iter().next() {
            let changed_modules: Vec<String>;
            {
                let mut server = self.server.write().await;
                changed_modules = server.update_module(module_name, &change.text);
            }
            for c in changed_modules {
                let uri = Self::module_name_to_uri(&c, &root);
                self.publish_diagnostics(&root, &uri).await;
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let root = Self::find_root(&uri).unwrap();
        let module_name = Self::uri_to_module_name(&uri, &root);

        let server = self.server.read().await;
        Ok(server
            .get_hover_info(&module_name, Self::from_lsp_position(position))
            .map(|h| self.convert_hover(h)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let root = Self::find_root(&uri).unwrap();
        let module_name = Self::uri_to_module_name(&uri, &root);

        let server = self.server.read().await;

        Ok(server
            .get_definition_location(&module_name, Self::from_lsp_position(position))
            .map(|DefinitionLocation { module, range }| {
                GotoDefinitionResponse::Scalar(Location {
                    uri: Self::module_name_to_uri(&module, &root),
                    range: Self::to_lsp_range(range),
                })
            }))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let root = Self::find_root(&uri).unwrap();
        let module_name = Self::uri_to_module_name(&uri, &root);

        let server = self.server.read().await;

        if let Some(renameable_symbol) =
            server.get_renameable_symbol(&module_name, Self::from_lsp_position(position))
        {
            Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: Self::to_lsp_range(renameable_symbol.range),
                placeholder: renameable_symbol.current_name,
            }))
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        let root = Self::find_root(&uri).unwrap();
        let module_name = Self::uri_to_module_name(&uri, &root);

        let server = self.server.read().await;

        if let Some(rename_locations) =
            server.get_rename_locations(&module_name, Self::from_lsp_position(position))
        {
            let mut changes: std::collections::HashMap<Url, Vec<TextEdit>> =
                std::collections::HashMap::new();

            for RenameLocation { module, range } in rename_locations {
                let file_uri = Self::module_name_to_uri(&module, &root);
                let edit = TextEdit {
                    range: Self::to_lsp_range(range),
                    new_text: new_name.clone(),
                };

                changes.entry(file_uri).or_default().push(edit);
            }

            Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }))
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub async fn execute() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(HopLanguageServer::new);
    LspServer::new(stdin, stdout, socket).serve(service).await;
}
