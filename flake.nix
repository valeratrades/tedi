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
              publishCachix = "valeratrades";
              containerRelease = { registry = "ghcr.io/valeratrades"; };
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

          build_rust = v_flakes.rs.build_nightly system;
          rustPlatform = pkgs.makeRustPlatform {
            rustc = build_rust;
            cargo = build_rust;
            inherit stdenv;
          };
          src = pkgs.lib.cleanSourceWith {
            src = pkgs.lib.cleanSource ./.;
            filter = path: _type: ! builtins.elem (baseNameOf path) [ "target" ".cargo" ]; # .cargo holds dev-only accelerators the sandbox lacks
          };
          tediBin = rustPlatform.buildRustPackage {
            inherit pname src;
            version = workspaceManifest.version;

            buildInputs = alwaysPkgs;
            nativeBuildInputs = with pkgs; [ pkg-config ];

            cargoLock.lockFile = ./Cargo.lock;

            RUSTC_WRAPPER = "";
            doCheck = false; # snapshot tests are colour-sensitive in the sandbox; CI test jobs run them
          };
          # The board's client, what `tedi web --public` serves.
          webBundle = rustPlatform.buildRustPackage {
            pname = "tedi_web-bundle";
            inherit src;
            version = workspaceManifest.version;
            cargoLock.lockFile = ./Cargo.lock;
            nativeBuildInputs = with pkgs; [ dioxus-cli wasm-bindgen-cli_0_2_129 tailwindcss_4 binaryen ];
            RUSTC_WRAPPER = "";
            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR"
              dx bundle --web --release -p tedi_web --out-dir "$TMPDIR/bundle"
              runHook postBuild
            '';
            installPhase = ''
              cp -r "$TMPDIR/bundle/public" "$out"
            '';
            doCheck = false;
          };
          # The team instance: its own store on the volume, git-initialized on first boot.
          board = pkgs.writeShellApplication {
            name = "tedi-board";
            runtimeInputs = [ tediBin pkgs.git pkgs.coreutils ];
            text = ''
              issues="$XDG_DATA_HOME/tedi/issues"
              if [ ! -d "$issues/.git" ]; then
                mkdir -p "$issues"
                git -C "$issues" init -q
                git -C "$issues" config user.name "tedi web"
                git -C "$issues" config user.email "tedi-web@localhost"
              fi
              exec tedi web --addr 0.0.0.0:7341 --public ${webBundle} --pull-every 10m
            '';
          };
          containerStd = v_flakes.container.implement {
            inherit pkgs pname;
            containers.web = {
              port = 7341;
              healthPath = "/";
              criticality = "normal";
              entrypoint = [ "${board}/bin/tedi-board" ];
              mounts = [ "/data" ];
              sqlite = [ ]; # the store is git and files, recovered from GitHub by the next pull
              workingDir = "/data";
              imageEnv = [ "HOME=/data" "XDG_DATA_HOME=/data" "XDG_STATE_HOME=/data/state" "XDG_CACHE_HOME=/data/cache" ];
              settings = {
                pkg = pname;
                values = {
                  github_token.file = "/secrets/GITHUB_TOKEN";
                  github_webhook_secret.file = "/secrets/GITHUB_WEBHOOK_SECRET";
                  watch = [ "EV-Main" "EV-Estate" "EV-Service-Arb" ];
                  board = {
                    archive_after = "1w";
                    views = [
                      {
                        name = "Triage";
                        query = "is:open stage:icebox,triage,selected,in_progress,in_review";
                        layout = "board";
                        group_by = "stage";
                        sort = "rank";
                        shown = [ "repo" "assignees" "importance" "hours" "type" "blockers" ];
                      }
                      {
                        name = "My issues";
                        query = "assignee:@me is:open";
                        layout = "list";
                        group_by = "stage";
                        sort = "rank";
                        shown = [ "repo" "stage" "importance" "hours" ];
                      }
                    ];
                    scopes = {
                      EV-Main.repos = [ "EV-Main/*" ];
                      EV-Estate.repos = [ "EV-Estate/*" ];
                      EV-Service-Arb.repos = [ "EV-Service-Arb/*" ];
                    };
                  };
                };
              };
            };
          };
          # Read by the devops generator beside `implement`'s contract, which refuses keys it does not know.
          deploy.ingress = {
            # GitHub delivers from a few shared IPs, so the generator's default of 10 a minute would throttle it.
            rateLimitPathPrefixes = [ "/webhook" ];
            rateLimit = { average = 300; period = "1m"; burst = 100; };
          };
        in
        {
          packages = { default = tediBin; inherit webBundle; } // containerStd.packages;
          containers = pkgs.lib.mapAttrs (_: c: c // { contract = c.contract // deploy; }) containerStd.containers;

          apps.help = {
            type = "app";
            program = "${pkgs.writeShellScriptBin "help" ''
              cat <<'EOF'
              ${pname} — GitHub issues as local markdown, plus sprints, blockers and daily stats.

              Commands:
                nix develop                    dev shell (rust nightly, pre-commit, generated configs)
                nix build                      build the `${pname}` binary
                nix run . -- --help            CLI help: open, sprints, manual, clockify, perf-eval, monitors, init
                cargo t                        tests (integration: tedi_task_operations/tests/)

              Outputs:
                homeManagerModules.monitors-watch   systemd user service running `${pname} monitors watch`

              Docs: README.md, docs/ARCHITECTURE.md, docs/spec/
              EOF
            ''}/bin/help";
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
                  dioxus-cli # `dx serve` / `dx bundle -p tedi_web`: the board's client
                  tailwindcss_4 # tedi_web's build.rs builds its stylesheet with it
                  wasm-bindgen-cli_0_2_129 # dx needs the CLI the crate graph pins (`tedi_web`: `wasm-bindgen = "=0.2.129"`)
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
