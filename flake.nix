{
  inputs = {
    v_flakes.url = "github:valeratrades/v_flakes?ref=v1.6";
  };

  outputs =
    { self, v_flakes }:
    let
      inherit (v_flakes) flake-utils pre-commit-hooks;
      manifest = (v_flakes.nixpkgs.lib.importTOML ./tedi/Cargo.toml).package;
      pname = manifest.name;
    in
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = import v_flakes.default_nixpkgs { inherit system; };
          rust = v_flakes.rs.default_nightly system;
          pre-commit-check = pre-commit-hooks.lib.${system}.run (v_flakes.files.preCommit { inherit pkgs; });
          workspaceManifest = (pkgs.lib.importTOML ./Cargo.toml).workspace.package;
          stdenv = pkgs.stdenvAdapters.useMoldLinker pkgs.stdenv;

          # Note: openssl.out and openssl.dev are auto-added by v_flakes for jobs
          alwaysPkgNames = [ "mold" "egl-wayland" "wayland" "libGL" "libgbm" ];
          alwaysPkgs = map (name: pkgs.${name}) alwaysPkgNames ++ [ pkgs.openssl.dev ];

          # v_flakes modules {{{1
          rs = v_flakes.rs {
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
            v_flakes.github {
              inherit pkgs pname rs;
              lastSupportedVersion = "nightly-${v_flakes.rs.nightly_version}";
              enable = true;
              jobs.default = true;
              jobs.errors.install = jobDeps;
              jobs.warnings.install = jobDeps;
              jobs.warnings.augment = [ "code-duplication" ];
              release.default = true;
              release.cargoTomlPath = "tedi/Cargo.toml";
              labels.extra = [
                { name = "milestones"; color = "0000ff"; }
                { name = "nuke"; color = "0000ff"; description = "remove something. Basically `chore` but strictly subtractive"; }
                { name = "daily_ev"; color = "0000ff"; description = "everything relevant to the `ev` command"; }
              ];
            };
          readme = v_flakes.readme-fw {
            inherit pkgs pname;
            defaults = true;
            lastSupportedVersion = "nightly-1.90";
            rootDir = ./.;
            badges = [ "msrv" "crates_io" "docs_rs" "loc" "ci" ];
          };
          combined = v_flakes.utils.combine { inherit rust; modules = [ rs github readme ]; };
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
                  cp -f ${(v_flakes.files.treefmt) { inherit pkgs; }} ./.treefmt.toml
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
