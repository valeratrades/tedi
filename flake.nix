{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/11707dc2f618dd54ca8739b309ec4fc024de578b";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix/f0927703b7b1c8d97511c4116eb9b4ec6645a0fa";
    v-utils.url = "github:valeratrades/v_flakes?ref=v1.6";
  };
  outputs = { self, flake-utils, pre-commit-hooks, v-utils }:
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = import v-utils.default_nixpkgs {
            inherit system;
            allowUnfree = true;
          };
          rust = v-utils.rs.default_nightly system;
          pre-commit-check = pre-commit-hooks.lib.${system}.run (v-utils.files.preCommit { inherit pkgs; });
          manifest = (pkgs.lib.importTOML ./tedi/Cargo.toml).package;
          workspaceManifest = (pkgs.lib.importTOML ./Cargo.toml).workspace.package;
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
                ignored_error = false;
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
              enable = true;
              jobs.default = true;
              jobs.errors.install = jobDeps;
              jobs.warnings.install = jobDeps;
              jobs.warnings.augment = [ "code-duplication" ];
              release.default = true;
              labels.extra = [
                { name = "milestones"; color = "0000ff"; }
                { name = "nuke"; color = "0000ff"; description = "remove something. Basically `chore` but strictly subtractive"; }
                { name = "daily_ev"; color = "0000ff"; description = "everything relevant to the `ev` command"; }
              ];
            };
          readme = v-utils.readme-fw {
            inherit pkgs pname;
            defaults = true;
            lastSupportedVersion = "nightly-1.90";
            rootDir = ./.;
            badges = [ "msrv" "crates_io" "docs_rs" "loc" "ci" ];
          };
          combined = v-utils.utils.combine { inherit rust; modules = [ rs github readme ]; };
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
                version = workspaceManifest.version;

                buildInputs = alwaysPkgs;
                nativeBuildInputs = with pkgs; [ pkg-config ];

                cargoLock.lockFile = ./Cargo.lock;
                src = pkgs.lib.cleanSource ./.;

                RUSTC_WRAPPER = ""; # .cargo/config.toml sets sccache, absent in sandbox
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
      homeManagerModules."monitors-watch" = { config, lib, pkgs, ... }:
        let
          inherit (lib) mkEnableOption mkOption mkIf;
          inherit (lib.types) package;
          cfg = config.services.todo-monitors-watch;
          manifest = (lib.importTOML ./tedi/Cargo.toml).package;
          pname = manifest.name;
        in
        {
          options.services.todo-monitors-watch = {
            enable = mkEnableOption "todo monitors watch daemon";

            package = mkOption {
              type = package;
              default = self.packages.${pkgs.system}.default;
              description = "The todo package to use.";
            };
          };

          config = mkIf cfg.enable {
            systemd.user.services.todo-monitors-watch = {
              Unit = {
                Description = "todo monitors watch daemon - periodic screenshot capture";
                After = [ "graphical-session.target" ];
              };

              Install = {
                WantedBy = [ "graphical-session.target" ];
              };

              Service = {
                Type = "simple";
                ExecStart = "${cfg.package}/bin/${pname} monitors watch";
                Restart = "on-failure";
                RestartSec = "10s";
              };
            };

            home.packages = [ cfg.package ];
          };
        };
    };
}
