use hop_core::document::{Document, DocumentRange};
use hop_core::document_id::DocumentId;
use hop_core::document_position::DocumentPosition;
use hop_core::program::{DefinitionLocation, Program, RenameLocation, Severity};
use hop_core::project::Project;
use std::collections::HashMap;
use tokio::sync::mpsc;
use tokio::sync::{OnceCell, RwLock};
use tower_lsp_server::LanguageServer;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{self, *};

pub enum ClientMessage {
    PublishDiagnostics {
        uri: Uri,
        diagnostics: Vec<Diagnostic>,
    },
    ShowMessage {
        message_type: MessageType,
        message: String,
    },
}

pub async fn execute() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        tower_lsp_server::LspService::new(|client: tower_lsp_server::Client| {
            let (tx, mut rx) = mpsc::channel::<ClientMessage>(32);

            tokio::spawn(async move {
                while let Some(msg) = rx.recv().await {
                    match msg {
                        ClientMessage::PublishDiagnostics { uri, diagnostics } => {
                            client.publish_diagnostics(uri, diagnostics, None).await;
                        }
                        ClientMessage::ShowMessage {
                            message_type,
                            message,
                        } => {
                            client.show_message(message_type, message).await;
                        }
                    }
                }
            });

            HopLanguageServer::new(tx)
        });

    tower_lsp_server::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}

// LSP uses UTF-16 encoding by default for position character offsets.
fn lsp_pos_to_doc_pos(lsp_pos: ls_types::Position) -> DocumentPosition {
    DocumentPosition::Utf16 {
        line: lsp_pos.line as usize,
        column: lsp_pos.character as usize,
    }
}

fn doc_range_to_lsp_range(range: DocumentRange) -> ls_types::Range {
    let start_pos = range.start_utf16();
    let end_pos = range.end_utf16();
    ls_types::Range {
        start: ls_types::Position {
            line: start_pos.line() as u32,
            character: start_pos.column() as u32,
        },
        end: ls_types::Position {
            line: end_pos.line() as u32,
            character: end_pos.column() as u32,
        },
    }
}

pub struct HopLanguageServer {
    client_tx: mpsc::Sender<ClientMessage>,
    program: RwLock<Program>,
    project: OnceCell<Project>,
}

impl HopLanguageServer {
    pub fn new(client_tx: mpsc::Sender<ClientMessage>) -> Self {
        Self {
            client_tx,
            program: RwLock::new(Program::default()),
            project: OnceCell::new(),
        }
    }

    fn uri_to_document_id(uri: &Uri, project: &Project) -> DocumentId {
        let path = uri.to_file_path().expect("URI is not a file path");
        project
            .path_to_document_id(&path)
            .expect("Failed to resolve module ID")
    }

    fn document_id_to_uri(document_id: &DocumentId, project: &Project) -> Uri {
        let p = project.document_id_to_path(document_id);
        Uri::from_file_path(&p).expect("Failed to create URI from file path")
    }

    async fn publish_diagnostics(&self, project: &Project, uri: &Uri) {
        let document_id = Self::uri_to_document_id(uri, project);
        let program = self.program.read().await;
        let diagnostics = program.get_error_diagnostics(document_id);

        let lsp_diagnostics: Vec<Diagnostic> = diagnostics
            .into_iter()
            .map(|d| Diagnostic {
                range: doc_range_to_lsp_range(d.range),
                severity: Some(match d.severity {
                    Severity::Error => DiagnosticSeverity::ERROR,
                    Severity::Warning => DiagnosticSeverity::WARNING,
                }),
                code: None,
                code_description: None,
                source: Some("hop".to_string()),
                message: d.message,
                related_information: None,
                tags: None,
                data: None,
            })
            .collect();

        let _ = self
            .client_tx
            .send(ClientMessage::PublishDiagnostics {
                uri: uri.clone(),
                diagnostics: lsp_diagnostics,
            })
            .await;
    }
}

impl LanguageServer for HopLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Try to find the project root from the rootUri or rootPath
        #[allow(deprecated)]
        if let Some(ref root_uri) = params.root_uri {
            if let Some(root_path) = root_uri.to_file_path() {
                let project_result = Project::find_traversing_superdirectories(&root_path)
                    .or_else(|_| Project::find_traversing_subdirectories(&root_path));

                match project_result {
                    Ok(proj) => {
                        let _ = self.project.set(proj);
                    }
                    Err(e) => {
                        let _ = self
                            .client_tx
                            .send(ClientMessage::ShowMessage {
                                message_type: MessageType::WARNING,
                                message: format!("Failed to load Hop project: {e}"),
                            })
                            .await;
                    }
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
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        if let Some(project) = self.project.get() {
            if let Ok(document_ids) = project.find_hop_modules() {
                {
                    let mut server = self.program.write().await;
                    for document_id in &document_ids {
                        if let Ok(document) = project.load_document(document_id) {
                            server.update_module(document_id, document);
                        }
                    }
                }
                for document_id in document_ids {
                    let uri = Self::document_id_to_uri(&document_id, project);
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
            let document_id = Self::uri_to_document_id(&uri, project);
            if let Some(change) = params.content_changes.into_iter().next() {
                let changed_modules: Vec<DocumentId>;
                {
                    let mut server = self.program.write().await;
                    changed_modules = server.update_module(
                        &document_id,
                        Document::new(document_id.clone(), change.text),
                    );
                }
                for c in changed_modules {
                    let uri = Self::document_id_to_uri(&c, project);
                    self.publish_diagnostics(project, &uri).await;
                }
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(project) = self.project.get() {
            let document_id = Self::uri_to_document_id(&uri, project);

            let program = self.program.read().await;
            Ok(program
                .get_hover_info(&document_id, lsp_pos_to_doc_pos(position))
                .map(|hover_info| Hover {
                    contents: HoverContents::Scalar(MarkedString::String(hover_info.message)),
                    range: Some(doc_range_to_lsp_range(hover_info.range)),
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
            let document_id = Self::uri_to_document_id(&uri, project);

            let program = self.program.read().await;

            Ok(program
                .get_definition_location(&document_id, lsp_pos_to_doc_pos(position))
                .map(|DefinitionLocation { range }| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: Self::document_id_to_uri(range.document_id(), project),
                        range: doc_range_to_lsp_range(range),
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
            let document_id = Self::uri_to_document_id(&uri, project);

            let program = self.program.read().await;

            if let Some(renameable_symbol) =
                program.get_renameable_symbol(&document_id, lsp_pos_to_doc_pos(position))
            {
                Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                    range: doc_range_to_lsp_range(renameable_symbol.range.clone()),
                    placeholder: renameable_symbol.current_name().to_string(),
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
            let document_id = Self::uri_to_document_id(&uri, project);

            let server = self.program.read().await;

            if let Some(rename_locations) =
                server.get_rename_locations(&document_id, lsp_pos_to_doc_pos(position))
            {
                #[allow(clippy::mutable_key_type)]
                let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

                for RenameLocation { range } in rename_locations {
                    let file_uri = Self::document_id_to_uri(range.document_id(), project);
                    let edit = TextEdit {
                        range: doc_range_to_lsp_range(range),
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
            let document_id = Self::uri_to_document_id(&uri, project);

            let program = self.program.read().await;

            match program.get_formatted_module(&document_id) {
                Ok(formatted) => Ok(Some(vec![TextEdit {
                    range: ls_types::Range {
                        start: ls_types::Position {
                            line: 0,
                            character: 0,
                        },
                        end: ls_types::Position {
                            line: u32::MAX,
                            character: 0,
                        },
                    },
                    new_text: formatted,
                }])),
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tempfile::TempDir;
    use txtar::{Archive, write_archive_to_dir};

    #[tokio::test]
    async fn test_initialize_resolves_project() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            type User {
                name: String
            }
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        let (tx, _rx) = mpsc::channel(32);
        let server = HopLanguageServer::new(tx);

        let root_uri = Uri::from_file_path(temp_dir.path()).unwrap();
        #[allow(deprecated)]
        let params = InitializeParams {
            root_uri: Some(root_uri),
            ..Default::default()
        };

        server.initialize(params).await.unwrap();

        let project = server.project.get().expect("project should be resolved");
        assert_eq!(project.get_project_root(), temp_dir.path().canonicalize().unwrap());
    }

    #[tokio::test]
    async fn test_initialize_resolves_project_in_subfolder() {
        let archive = Archive::from(indoc! {r#"
            -- hop/hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- hop/main.hop --
            type User {
                name: String
            }
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        let (tx, _rx) = mpsc::channel(32);
        let server = HopLanguageServer::new(tx);

        let root_uri = Uri::from_file_path(temp_dir.path()).unwrap();
        #[allow(deprecated)]
        let params = InitializeParams {
            root_uri: Some(root_uri),
            ..Default::default()
        };

        server.initialize(params).await.unwrap();

        let project = server.project.get().expect("project should be resolved");
        assert_eq!(project.get_project_root(), temp_dir.path().join("hop").canonicalize().unwrap());
    }

    #[tokio::test]
    async fn test_initialize_shows_warning_when_project_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- main.hop --
            type User {
                name: String
            }
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        let (tx, mut rx) = mpsc::channel(32);
        let server = HopLanguageServer::new(tx);

        let root_uri = Uri::from_file_path(temp_dir.path()).unwrap();
        #[allow(deprecated)]
        let params = InitializeParams {
            root_uri: Some(root_uri),
            ..Default::default()
        };

        server.initialize(params).await.unwrap();

        assert!(server.project.get().is_none());

        let msg = rx.recv().await.expect("should receive a warning message");
        match msg {
            ClientMessage::ShowMessage {
                message_type,
                message,
            } => {
                assert_eq!(message_type, MessageType::WARNING);
                assert!(message.contains("Failed to load Hop project"));
            }
            _ => panic!("expected ShowMessage"),
        }
    }
}
