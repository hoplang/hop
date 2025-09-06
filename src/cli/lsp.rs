use crate::filesystem::files::{self as files, ProjectRoot};
use crate::hop::program::{DefinitionLocation, Program, RenameLocation};
use crate::range::Position;
use crate::range::string_cursor::StringSpan;
use std::path::Path;
use tokio::sync::{OnceCell, RwLock};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server as LspServer};

// These conversions assume that a UTF-8 PositionEncodingKind has been negotiated
impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(position: tower_lsp::lsp_types::Position) -> Self {
        Position::new(position.line as usize, position.character as usize)
    }
}

impl From<Position> for tower_lsp::lsp_types::Position {
    fn from(position: Position) -> Self {
        tower_lsp::lsp_types::Position {
            line: position.line() as u32,
            character: position.column() as u32,
        }
    }
}

impl From<StringSpan> for tower_lsp::lsp_types::Range {
    fn from(span: StringSpan) -> Self {
        tower_lsp::lsp_types::Range {
            start: span.start().into(),
            end: span.end().into(),
        }
    }
}

pub struct HopLanguageServer {
    client: Client,
    program: RwLock<Program>,
    root: OnceCell<ProjectRoot>,
}

impl HopLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            program: RwLock::new(Program::new()),
            root: OnceCell::new(),
        }
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

    async fn publish_diagnostics(&self, root: &ProjectRoot, uri: &Url) {
        let module_name = Self::uri_to_module_name(uri, root);
        let program = self.program.read().await;
        let diagnostics = program.get_error_diagnostics(&module_name);

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = diagnostics
            .into_iter()
            .map(|d| tower_lsp::lsp_types::Diagnostic {
                range: d.span.into(),
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
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Try to find the project root from the rootUri or rootPath
        if let Some(root_uri) = params.root_uri {
            if let Ok(root_path) = root_uri.to_file_path() {
                if let Ok(project_root) = ProjectRoot::find_upwards(&root_path) {
                    let _ = self.root.set(project_root);
                }
            }
        }
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

    async fn initialized(&self, _: InitializedParams) {
        if let Some(root) = self.root.get() {
            if let Ok(all_modules) = files::load_all_hop_modules(root) {
                let names: Vec<String> = all_modules.keys().cloned().collect();
                {
                    let mut server = self.program.write().await;
                    for (module_name, content) in all_modules {
                        server.update_module(&module_name, content);
                    }
                }
                for module_name in names {
                    let uri = Self::module_name_to_uri(&module_name, root);
                    self.publish_diagnostics(root, &uri).await;
                }
            }
        }
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn did_open(&self, _params: DidOpenTextDocumentParams) {}

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(root) = self.root.get() {
            let module_name = Self::uri_to_module_name(&uri, root);
            if let Some(change) = params.content_changes.into_iter().next() {
                let changed_modules: Vec<String>;
                {
                    let mut server = self.program.write().await;
                    changed_modules = server.update_module(&module_name, change.text);
                }
                for c in changed_modules {
                    let uri = Self::module_name_to_uri(&c, root);
                    self.publish_diagnostics(root, &uri).await;
                }
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(root) = self.root.get() {
            let module_name = Self::uri_to_module_name(&uri, root);

            let program = self.program.read().await;
            Ok(program
                .get_hover_info(&module_name, position.into())
                .map(|hover_info| Hover {
                    contents: HoverContents::Scalar(MarkedString::String(hover_info.type_str)),
                    range: Some(hover_info.span.into()),
                }))
        } else {
            Ok(None)
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(root) = self.root.get() {
            let module_name = Self::uri_to_module_name(&uri, root);

            let program = self.program.read().await;

            Ok(program
                .get_definition_location(&module_name, position.into())
                .map(|DefinitionLocation { module, span }| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: Self::module_name_to_uri(&module, root),
                        range: span.into(),
                    })
                }))
        } else {
            Ok(None)
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        if let Some(root) = self.root.get() {
            let module_name = Self::uri_to_module_name(&uri, root);

            let program = self.program.read().await;

            if let Some(renameable_symbol) =
                program.get_renameable_symbol(&module_name, position.into())
            {
                Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                    range: renameable_symbol.span.into(),
                    placeholder: renameable_symbol.current_name,
                }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        if let Some(root) = self.root.get() {
            let module_name = Self::uri_to_module_name(&uri, root);

            let server = self.program.read().await;

            if let Some(rename_locations) =
                server.get_rename_locations(&module_name, position.into())
            {
                let mut changes: std::collections::HashMap<Url, Vec<TextEdit>> =
                    std::collections::HashMap::new();

                for RenameLocation { module, span } in rename_locations {
                    let file_uri = Self::module_name_to_uri(&module, root);
                    let edit = TextEdit {
                        range: span.into(),
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
