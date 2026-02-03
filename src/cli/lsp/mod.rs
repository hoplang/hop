mod server;

use server::HopLanguageServer;
use tower_lsp_server::{LspService, Server as LspServer};

pub async fn execute() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(HopLanguageServer::new);
    LspServer::new(stdin, stdout, socket).serve(service).await;
}
