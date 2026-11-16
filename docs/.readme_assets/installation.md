```sh
nix build
```

# Configuration

Create `~/.config/todo/settings.toml`:

```toml
# Required for GitHub integration: a token, or a GitHub App (one rate budget per installation)
github_token = "ghp_..."
# [github_app]
# id = 123456
# private_key = { file = "/run/secrets/app.pem" }

# Required for Clockify integration
clockify_api_key = "..."
clockify_workspace_id = "..."

# Optional
default_extension = "md"  # or "typ" for typst
github_max_in_flight = 32  # GitHub requests in flight at once
```
