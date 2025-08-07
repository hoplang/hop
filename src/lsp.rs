use crate::files::{self, ProjectRoot};
use crate::server::{HoverInfo, Server};
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
            server: RwLock::new(Server::new()),
        }
    }

    // Convert from LSP position (0-based) to Rust Position (1-based)
    fn from_lsp_position(position: tower_lsp::lsp_types::Position) -> (usize, usize) {
        (position.line as usize + 1, position.character as usize + 1)
    }

    // Convert from Rust position (1-based) to LSP position (0-based)
    fn to_lsp_position(line: usize, column: usize) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position {
            line: (line - 1) as u32,
            character: (column - 1) as u32,
        }
    }

    fn find_root(&self, uri: &Url) -> anyhow::Result<ProjectRoot> {
        ProjectRoot::find_upwards(std::path::Path::new(uri.path()))
    }

    fn uri_to_module_name(&self, uri: &Url, root: &ProjectRoot) -> String {
        match files::path_to_module_name(Path::new(uri.path()), &root) {
            Ok(s) => s,
            Err(_) => "error".to_string(),
        }
    }

    fn convert_hover(&self, hover_info: HoverInfo) -> Hover {
        Hover {
            contents: HoverContents::Scalar(MarkedString::String(hover_info.type_str)),
            range: Some(Range {
                start: Self::to_lsp_position(hover_info.start_line, hover_info.start_column),
                end: Self::to_lsp_position(hover_info.end_line, hover_info.end_column),
            }),
        }
    }

    async fn load_modules(&self, root: &ProjectRoot) -> std::io::Result<()> {
        // Load all hop modules from the base directory
        let all_modules = files::load_all_hop_modules(&root)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        // Now load all the modules we found
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Found {} hop modules to potentially load",
                    all_modules.len()
                ),
            )
            .await;

        for (module_name, content) in all_modules {
            // Check if we already have this module loaded
            {
                let server = self.server.read().await;
                if server.has_module(&module_name) {
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("Module '{}' already loaded, skipping", module_name),
                        )
                        .await;
                    continue;
                }
            }

            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Loading module '{}' ({} chars)", module_name, content.len()),
                )
                .await;

            let mut server = self.server.write().await;
            server.update_module(module_name, &content);
        }

        Ok(())
    }

    async fn publish_diagnostics(&self, root: &ProjectRoot, uri: &Url) {
        let module_name = self.uri_to_module_name(uri, &root);
        let server = self.server.read().await;
        let diagnostics = server.get_error_diagnostics(&module_name);

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = diagnostics
            .into_iter()
            .map(|d| tower_lsp::lsp_types::Diagnostic {
                range: Range {
                    start: Self::to_lsp_position(d.start_line, d.start_column),
                    end: Self::to_lsp_position(d.end_line, d.end_column),
                },
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
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "hop-language-server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Hop language server initialized")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let root = self.find_root(&uri).unwrap();
        let module_name = self.uri_to_module_name(&uri, &root);

        self.client
            .log_message(
                MessageType::INFO,
                format!("Opening file: {} (module: {})", uri.path(), module_name),
            )
            .await;

        // Load any missing dependency modules from the same directory
        self.client
            .log_message(MessageType::INFO, "Loading dependency modules...")
            .await;
        let _ = self.load_modules(&root).await;

        {
            let mut server = self.server.write().await;
            server.update_module(module_name.clone(), &text);
        }

        self.client
            .log_message(
                MessageType::INFO,
                format!("Updated module '{}', publishing diagnostics", module_name),
            )
            .await;

        self.publish_diagnostics(&root, &uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let root = self.find_root(&uri).unwrap();
        let module_name = self.uri_to_module_name(&uri, &root);
        let changes = params.content_changes;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Changing file: {} (module: {})", uri.path(), module_name),
            )
            .await;

        if let Some(change) = changes.into_iter().next() {
            let text = change.text;

            {
                let mut server = self.server.write().await;
                server.update_module(module_name, &text);
            }

            self.publish_diagnostics(&root, &uri).await;
        }
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {}

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let root = self.find_root(&uri).unwrap();
        let module_name = self.uri_to_module_name(&uri, &root);

        let (line, column) = Self::from_lsp_position(position);
        let server = self.server.read().await;
        Ok(server
            .get_hover_info(&module_name, line, column)
            .map(|h| self.convert_hover(h)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let root = self.find_root(&uri).unwrap();
        let module_name = self.uri_to_module_name(&uri, &root);

        let (line, column) = Self::from_lsp_position(position);
        let server = self.server.read().await;

        if let Some(definition) = server.get_definition(&module_name, line, column) {
            let definition_uri = {
                let def_path = files::module_name_to_path(&definition.module, &root);
                match Url::from_file_path(&def_path) {
                    Ok(url) => url,
                    Err(_) => return Ok(None),
                }
            };

            let location = Location {
                uri: definition_uri,
                range: Range {
                    start: Self::to_lsp_position(definition.start_line, definition.start_column),
                    end: Self::to_lsp_position(definition.end_line, definition.end_column),
                },
            };

            Ok(Some(GotoDefinitionResponse::Scalar(location)))
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub async fn run_lsp() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(HopLanguageServer::new);
    LspServer::new(stdin, stdout, socket).serve(service).await;
}
