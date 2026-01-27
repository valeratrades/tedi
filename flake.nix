{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    v-utils.url = "github:valeratrades/.github?ref=v1.4";
  };
  outputs = { self, nixpkgs, rust-overlay, flake-utils, pre-commit-hooks, v-utils }:
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs {
            inherit system overlays;
            allowUnfree = true;
          };
          #NB: can't load rust-bin from nightly.latest, as there are week guarantees of which components will be available on each day.
          rust = pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
            extensions = [ "rust-src" "rust-analyzer" "rust-docs" "rustc-codegen-cranelift-preview" ];
          });
          pre-commit-check = pre-commit-hooks.lib.${system}.run (v-utils.files.preCommit { inherit pkgs; });
          manifest = (pkgs.lib.importTOML ./Cargo.toml).package;
          pname = manifest.name;
          stdenv = pkgs.stdenvAdapters.useMoldLinker pkgs.stdenv;

          # Shared runtime dependencies
          # Note: openssl.out and openssl.dev are auto-added by v-utils for jobs
          alwaysPkgNames = [ "mold" "egl-wayland" "wayland" "libGL" "libgbm" ];
          alwaysPkgs = map (name: pkgs.${name}) alwaysPkgNames ++ [ pkgs.openssl.dev ];

          # v-utils modules {{{1
          rs = v-utils.rs {
            inherit pkgs rust;
            deny = true;
            tracey = true;
            style = {
              modules = {
                ignored_error_comment = false; #TODO: fix them all here or add comments explaining why they're valid // or update the logic in codestyle to not warn on some always-valid patterns like (unwrap_or_else(|| panic!...)
              };
            };
          };
          github =
            let
              jobDeps = { packages = alwaysPkgNames ++ [ "fd" "pkg-config" ]; debug = true; };
            in
            v-utils.github {
              inherit pkgs pname rs;
              lastSupportedVersion = "nightly-2025-08-01";
              langs = [ "rs" ];
              jobs.default = true;
              jobs.errors.install = jobDeps;
              jobs.warnings.install = jobDeps;
              jobs.warnings.augment = [ "code-duplication" ];
              release.default = true;
            };
          readme = v-utils.readme-fw {
            inherit pkgs pname;
            defaults = true;
            lastSupportedVersion = "nightly-1.90";
            rootDir = ./.;
            badges = [ "msrv" "crates_io" "docs_rs" "loc" "ci" ];
          };
          combined = v-utils.utils.combine [ rs github readme ];
          #,}}}1
        in
        {
          packages =
            let
              rustc = rust;
              cargo = rust;
              rustPlatform = pkgs.makeRustPlatform {
                inherit rustc cargo stdenv;
              };
            in
            {
              default = rustPlatform.buildRustPackage {
                inherit pname;
                version = manifest.version;

                buildInputs = alwaysPkgs;
                nativeBuildInputs = with pkgs; [ pkg-config ];

                cargoLock.lockFile = ./Cargo.lock;
                src = pkgs.lib.cleanSource ./.;
              };
            };

          devShells.default =
            with pkgs;
            mkShell {
              inherit stdenv;
              shellHook =
                pre-commit-check.shellHook +
                combined.shellHook +
                ''
                  cp -f ${(v-utils.files.treefmt) { inherit pkgs; }} ./.treefmt.toml

                  #DEPRECATE: once GHA is working
                  # qlty
                  if [ ! -f "$HOME/.qlty/bin/qlty" ]; then
                    echo "Installing qlty..."
                    curl -fsSL https://qlty.sh | sh
                  fi
                  export PATH="$HOME/.qlty/bin:$PATH"
                '';
              packages =
                alwaysPkgs ++
                [
                  rust
                  pkg-config
                ] ++ pre-commit-check.enabledPackages ++ combined.enabledPackages;

              env.RUST_BACKTRACE = 1;
              env.RUST_LIB_BACKTRACE = 0;
            };
        }
      )
    // {
      homeManagerModules."watch-monitors" = { config, lib, pkgs, ... }:
        let
          inherit (lib) mkEnableOption mkOption mkIf;
          inherit (lib.types) package;
          cfg = config.services.todo-watch-monitors;
          manifest = (lib.importTOML ./Cargo.toml).package;
          pname = manifest.name;
        in
        {
          options.services.todo-watch-monitors = {
            enable = mkEnableOption "todo watch-monitors daemon";

            package = mkOption {
              type = package;
              default = self.packages.${pkgs.system}.default;
              description = "The todo package to use.";
            };
          };

          config = mkIf cfg.enable {
            systemd.user.services.todo-watch-monitors = {
              Unit = {
                Description = "todo watch-monitors daemon - periodic screenshot capture";
                After = [ "graphical-session.target" ];
              };

              Install = {
                WantedBy = [ "graphical-session.target" ];
              };

              Service = {
                Type = "simple";
                ExecStart = "${cfg.package}/bin/${pname} watch-monitors";
                Restart = "on-failure";
                RestartSec = "10s";
              };
            };

            home.packages = [ cfg.package ];
          };
        };
    };
}
