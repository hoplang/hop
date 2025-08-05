use crate::files::{self, ProjectRoot};
use crate::server::Server;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server as LspServer};

pub struct HopLanguageServer {
    client: Client,
    server: Arc<RwLock<Server>>,
    document_map: Arc<RwLock<HashMap<Url, String>>>,
}

impl HopLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            server: Arc::new(RwLock::new(Server::new())),
            document_map: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    // Convert from LSP position (0-based) to Rust Position (1-based)
    fn lsp_position_to_rust(position: tower_lsp::lsp_types::Position) -> (usize, usize) {
        (position.line as usize + 1, position.character as usize + 1)
    }

    // Convert from Rust position (1-based) to LSP position (0-based)
    fn rust_position_to_lsp(line: usize, column: usize) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position {
            line: (line - 1) as u32,
            character: (column - 1) as u32,
        }
    }

    fn find_root(&self, uri: &Url) -> Option<ProjectRoot> {
        let file_path = std::path::Path::new(uri.path());
        ProjectRoot::find(file_path)
    }

    fn uri_to_module_name(&self, uri: &Url) -> String {
        let file_path = std::path::Path::new(uri.path());

        // Find the build.hop file to determine base directory
        if let Some(root) = self.find_root(uri) {
            if let Ok(module_name) = files::path_to_module_name(file_path, &root) {
                return module_name;
            }
        }

        // Fallback to just the file stem if we can't find build.hop
        file_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string()
    }

    async fn load_dependency_modules(&self, uri: &Url) -> std::io::Result<()> {
        // Find the build.hop file to determine base directory
        let root = self.find_root(uri).expect("Could not find hop root");

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

    async fn publish_diagnostics(&self, uri: &Url) {
        let module_name = self.uri_to_module_name(uri);
        let server = self.server.read().await;
        let diagnostics = server.get_error_diagnostics(&module_name);

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = diagnostics
            .into_iter()
            .map(|d| tower_lsp::lsp_types::Diagnostic {
                range: Range {
                    start: Self::rust_position_to_lsp(d.start_line, d.start_column),
                    end: Self::rust_position_to_lsp(d.end_line, d.end_column),
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
        let module_name = self.uri_to_module_name(&uri);

        self.client
            .log_message(
                MessageType::INFO,
                format!("Opening file: {} (module: {})", uri.path(), module_name),
            )
            .await;

        {
            let mut document_map = self.document_map.write().await;
            document_map.insert(uri.clone(), text.clone());
        }

        // Load any missing dependency modules from the same directory
        self.client
            .log_message(MessageType::INFO, "Loading dependency modules...")
            .await;
        let _ = self.load_dependency_modules(&uri).await;

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

        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        if let Some(change) = changes.into_iter().next() {
            let text = change.text;
            let module_name = self.uri_to_module_name(&uri);

            {
                let mut document_map = self.document_map.write().await;
                document_map.insert(uri.clone(), text.clone());
            }

            {
                let mut server = self.server.write().await;
                server.update_module(module_name, &text);
            }

            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut document_map = self.document_map.write().await;
        document_map.remove(&uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let module_name = self.uri_to_module_name(&uri);

        let (line, column) = Self::lsp_position_to_rust(position);
        let server = self.server.read().await;
        if let Some(hover_info) = server.get_hover_info(&module_name, line, column) {
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_info.type_str)),
                range: Some(Range {
                    start: Self::rust_position_to_lsp(
                        hover_info.start_line,
                        hover_info.start_column,
                    ),
                    end: Self::rust_position_to_lsp(hover_info.end_line, hover_info.end_column),
                }),
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
        let module_name = self.uri_to_module_name(&uri);

        let (line, column) = Self::lsp_position_to_rust(position);
        let server = self.server.read().await;

        if let Some(definition) = server.get_definition(&module_name, line, column) {
            // Convert the definition module name to a URI
            let def_uri = if definition.module == module_name {
                // Same module
                uri.clone()
            } else {
                // Different module - construct URI from module name using base directory
                if let Some(root) = self.find_root(&uri) {
                    let def_path = files::module_name_to_path(&definition.module, &root);

                    match Url::from_file_path(&def_path) {
                        Ok(url) => url,
                        Err(_) => return Ok(None),
                    }
                } else {
                    // Fallback to old behavior if no build.hop found
                    let current_path = std::path::Path::new(uri.path());
                    let parent_dir = current_path.parent().unwrap_or(std::path::Path::new("."));
                    let def_path = parent_dir.join(format!("{}.hop", definition.module));

                    match Url::from_file_path(&def_path) {
                        Ok(url) => url,
                        Err(_) => return Ok(None),
                    }
                }
            };

            let location = Location {
                uri: def_uri,
                range: Range {
                    start: Self::rust_position_to_lsp(
                        definition.start_line,
                        definition.start_column,
                    ),
                    end: Self::rust_position_to_lsp(definition.end_line, definition.end_column),
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
