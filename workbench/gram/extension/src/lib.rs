use zed_extension_api::{self as zed, Result};

struct HopExtension;

impl zed::Extension for HopExtension {
    fn new() -> Self {
        HopExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let command = worktree
            .which("hop")
            .ok_or_else(|| "hop was not found on $PATH: install it and restart Zed".to_string())?;

        Ok(zed::Command {
            command,
            args: vec!["lsp".to_string()],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(HopExtension);
