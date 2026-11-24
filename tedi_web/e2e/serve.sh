#!/usr/bin/env bash
# serve.sh [arg...]   the board over the seeded mock store on E2E_PORT, built, seeded and started when it isn't; args go to `tedi web`
# serve.sh stop       stops it
# env: E2E_HOME (store, default /tmp/tedi-e2e) E2E_PORT (7351) E2E_RESEED=1 E2E_REBUILD=1 (tedi and the client bundle) TEDI_BIN
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "$here/../.." && pwd)
. "$here/env.sh"
port=${E2E_PORT:-7351}
pidfile=$E2E_HOME/server.pid
alive() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }

if [ "${1:-}" = stop ]; then
  alive && kill "$(cat "$pidfile")"
  rm -f "$pidfile"
  exit 0
fi

in_dev() { (cd "$repo" && env -u XDG_DATA_HOME -u XDG_STATE_HOME -u XDG_CACHE_HOME nix develop -c "$@"); } # env.sh's cache dir is wiped by seed.sh, and kache and nix keep theirs in it
tedi_bin=${TEDI_BIN:-$repo/target/debug/tedi}
if [ ! -x "$tedi_bin" ] || [ -n "${E2E_REBUILD:-}" ]; then in_dev cargo b -p tedi >&2; fi
if [ ! -d "$here/.bundle/public" ] || [ -n "${E2E_REBUILD:-}" ]; then
  in_dev dx bundle --web -p tedi_web --out-dir "$here/.bundle" >&2
fi

if [ -n "${E2E_RESEED:-}" ] || [ ! -d "$XDG_DATA_HOME/tedi/issues/service-arb" ]; then
  alive && kill "$(cat "$pidfile")"
  TEDI_BIN=$tedi_bin "$here/seed.sh"
fi
if ! alive || [ -n "${E2E_REBUILD:-}" ]; then
  alive && kill "$(cat "$pidfile")" && sleep 1
  "$tedi_bin" --mock --config "$here/config.toml" web --addr 127.0.0.1:$port --public "$here/.bundle/public" "$@" >"$E2E_HOME/server.log" 2>&1 &
  echo $! >"$pidfile"
  for _ in $(seq 50); do curl -sf "http://127.0.0.1:$port/" >/dev/null && exit 0; sleep 0.2; done
  tail "$E2E_HOME/server.log" >&2
  exit 1
fi
