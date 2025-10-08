use crate::document::DocumentPosition;
use crate::document::document_cursor::DocumentRange;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::module_name::ModuleName;
use crate::hop::program::{DefinitionLocation, Program, RenameLocation};
use std::collections::HashMap;
use tokio::sync::{OnceCell, RwLock};
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::{self, *};
use tower_lsp_server::{Client, LanguageServer, LspService, Server as LspServer, UriExt};

// LSP uses UTF-16 encoding by default for position character offsets.
impl From<lsp_types::Position> for DocumentPosition {
    fn from(lsp_pos: lsp_types::Position) -> Self {
        DocumentPosition::Utf16 {
            line: lsp_pos.line as usize,
            column: lsp_pos.character as usize,
        }
    }
}

impl From<DocumentRange> for lsp_types::Range {
    fn from(range: DocumentRange) -> Self {
        let start_pos = range.start_utf16();
        let end_pos = range.end_utf16();

        lsp_types::Range {
            start: lsp_types::Position {
                line: start_pos.line() as u32,
                character: start_pos.column() as u32,
            },
            end: lsp_types::Position {
                line: end_pos.line() as u32,
                character: end_pos.column() as u32,
            },
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
            program: RwLock::new(Program::default()),
            root: OnceCell::new(),
        }
    }

    fn uri_to_module_name(uri: &Uri, root: &ProjectRoot) -> ModuleName {
        match uri.to_file_path() {
            Some(path) => match root.path_to_module_name(&path) {
                Ok(s) => s,
                Err(_) => panic!(),
            },
            None => panic!(),
        }
    }

    fn module_name_to_uri(name: &ModuleName, root: &ProjectRoot) -> Uri {
        let p = root.module_name_to_path(name);
        Uri::from_file_path(&p).expect("Failed to create URI from file path")
    }

    async fn publish_diagnostics(&self, root: &ProjectRoot, uri: &Uri) {
        let module_name = Self::uri_to_module_name(uri, root);
        let program = self.program.read().await;
        let diagnostics = program.get_error_diagnostics(module_name);

        let lsp_diagnostics: Vec<tower_lsp_server::lsp_types::Diagnostic> = diagnostics
            .into_iter()
            .map(|d| tower_lsp_server::lsp_types::Diagnostic {
                range: d.range.into(),
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

impl LanguageServer for HopLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Try to find the project root from the rootUri or rootPath
        #[allow(deprecated)]
        if let Some(root_uri) = params.root_uri {
            if let Some(root_path) = root_uri.to_file_path() {
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
            if let Ok(all_modules) = root.load_all_hop_modules() {
                let names: Vec<ModuleName> = all_modules.keys().cloned().collect();
                {
                    let mut server = self.program.write().await;
                    for (module_name, content) in all_modules {
                        server.update_module(module_name, content);
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
                let changed_modules: Vec<ModuleName>;
                {
                    let mut server = self.program.write().await;
                    changed_modules = server.update_module(module_name, change.text);
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
                    contents: HoverContents::Scalar(MarkedString::String(hover_info.message)),
                    range: Some(hover_info.range.into()),
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
                .map(|DefinitionLocation { module, range }| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: Self::module_name_to_uri(&module, root),
                        range: range.into(),
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
                    range: renameable_symbol.range.clone().into(),
                    placeholder: renameable_symbol.current_name.to_string(),
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
                #[allow(clippy::mutable_key_type)]
                let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

                for RenameLocation { module, range } in rename_locations {
                    let file_uri = Self::module_name_to_uri(&module, root);
                    let edit = TextEdit {
                        range: range.into(),
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
