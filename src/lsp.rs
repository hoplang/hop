use crate::server::Server;
use std::collections::HashMap;
use std::path::Path;
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

    fn uri_to_module_name(&self, uri: &Url) -> String {
        let path = std::path::Path::new(uri.path());
        path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string()
    }

    async fn load_dependency_modules(&self, uri: &Url) -> std::io::Result<()> {
        let file_path = Path::new(uri.path());
        let directory = file_path.parent().ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Could not find parent directory",
            )
        })?;

        // Helper function to load .hop files from a directory
        let load_from_directory = |dir_path: &Path| -> std::io::Result<Vec<(String, String)>> {
            let mut modules = Vec::new();

            if !dir_path.exists() || !dir_path.is_dir() {
                return Ok(modules);
            }

            let entries = std::fs::read_dir(dir_path)?;

            for entry in entries {
                let entry = entry?;
                let path = entry.path();

                if path.extension().and_then(|s| s.to_str()) == Some("hop") {
                    let module_name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string();

                    // Load the module content
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        modules.push((module_name, content));
                    }
                }
            }

            Ok(modules)
        };

        // Read all .hop files in the same directory as the current file
        let mut all_modules = load_from_directory(directory)?;

        // Also look for hop files in a "hop" subdirectory
        let hop_subdir = directory.join("hop");
        all_modules.extend(load_from_directory(&hop_subdir)?);

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
