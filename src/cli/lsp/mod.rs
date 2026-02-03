mod server;

use server::{ClientMessage, HopLanguageServer};
use tokio::sync::mpsc;
use tower_lsp_server::{Client, LspService, Server as LspServer};

pub async fn execute() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client: Client| {
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

    LspServer::new(stdin, stdout, socket).serve(service).await;
}
