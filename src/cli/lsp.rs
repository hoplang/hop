use crate::document::DocumentPosition;
use crate::document::{Document, DocumentRange};
use crate::filesystem::project::Project;
use crate::hop::program::{DefinitionLocation, Program, RenameLocation};
use crate::hop::symbols::module_id::ModuleId;
use crate::hop::syntax::format;
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
    project: OnceCell<Project>,
}

impl HopLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            program: RwLock::new(Program::default()),
            project: OnceCell::new(),
        }
    }

    fn uri_to_module_id(uri: &Uri, project: &Project) -> ModuleId {
        match uri.to_file_path() {
            Some(path) => match project.path_to_module_id(&path) {
                Ok(s) => s,
                Err(_) => panic!(),
            },
            None => panic!(),
        }
    }

    fn module_id_to_uri(module_id: &ModuleId, project: &Project) -> Uri {
        let p = project.module_id_to_path(module_id);
        Uri::from_file_path(&p).expect("Failed to create URI from file path")
    }

    async fn publish_diagnostics(&self, project: &Project, uri: &Uri) {
        let module_id = Self::uri_to_module_id(uri, project);
        let program = self.program.read().await;
        let diagnostics = program.get_error_diagnostics(module_id);

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
                if let Ok(proj) = Project::find_upwards(&root_path) {
                    let _ = self.project.set(proj);
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
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "hop-language-server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        if let Some(project) = self.project.get() {
            if let Ok(module_ids) = project.find_modules() {
                {
                    let mut server = self.program.write().await;
                    for module_id in &module_ids {
                        if let Ok(document) = project.load_module(module_id) {
                            server.update_module(module_id.clone(), document);
                        }
                    }
                }
                for module_id in module_ids {
                    let uri = Self::module_id_to_uri(&module_id, project);
                    self.publish_diagnostics(project, &uri).await;
                }
            }
        }
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn did_open(&self, _params: DidOpenTextDocumentParams) {}

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);
            if let Some(change) = params.content_changes.into_iter().next() {
                let changed_modules: Vec<ModuleId>;
                {
                    let mut server = self.program.write().await;
                    changed_modules = server.update_module(module_id, Document::new(change.text));
                }
                for c in changed_modules {
                    let uri = Self::module_id_to_uri(&c, project);
                    self.publish_diagnostics(project, &uri).await;
                }
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);

            let program = self.program.read().await;
            Ok(program
                .get_hover_info(&module_id, position.into())
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
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);

            let program = self.program.read().await;

            Ok(program
                .get_definition_location(&module_id, position.into())
                .map(|DefinitionLocation { module, range }| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: Self::module_id_to_uri(&module, project),
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
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);

            let program = self.program.read().await;

            if let Some(renameable_symbol) =
                program.get_renameable_symbol(&module_id, position.into())
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
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);

            let server = self.program.read().await;

            if let Some(rename_locations) = server.get_rename_locations(&module_id, position.into())
            {
                #[allow(clippy::mutable_key_type)]
                let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

                for RenameLocation { module, range } in rename_locations {
                    let file_uri = Self::module_id_to_uri(&module, project);
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

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        if let Some(project) = self.project.get() {
            let module_id = Self::uri_to_module_id(&uri, project);

            let program = self.program.read().await;

            // Don't format if there are parse errors
            if let Some(errors) = program.get_parse_errors().get(&module_id) {
                if !errors.is_empty() {
                    return Ok(None);
                }
            }

            if let Some(ast) = program.get_parsed_ast(&module_id) {
                let formatted = format(ast.clone());

                // Replace the entire document
                Ok(Some(vec![TextEdit {
                    range: lsp_types::Range {
                        start: lsp_types::Position {
                            line: 0,
                            character: 0,
                        },
                        end: lsp_types::Position {
                            line: u32::MAX,
                            character: 0,
                        },
                    },
                    new_text: formatted,
                }]))
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
