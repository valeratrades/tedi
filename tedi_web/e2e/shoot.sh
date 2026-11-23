#!/usr/bin/env bash
# shoot.sh <screen> [out.png]   screens: 01-board 02-view 03-library 04-workspace 05-scope 06a-mobile 06b-mobile
# shoot.sh stop                 stops the server it started
# env: E2E_HOME (store, default /tmp/tedi-e2e) E2E_PORT (7351) E2E_RESEED=1 E2E_REBUILD=1 (client bundle) TEDI_BIN
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
. "$here/env.sh"
port=${E2E_PORT:-7351}
pidfile=$E2E_HOME/server.pid

if [ "${1:-}" = stop ]; then
  [ -f "$pidfile" ] && kill "$(cat "$pidfile")" 2>/dev/null; rm -f "$pidfile"; exit 0
fi
screen=${1:?screen}
out=${2:-$here/out/$screen.png}
[ -f "$here/libraries/$screen.json" ] || { echo "no library for $screen" >&2; exit 1; }
mkdir -p "$(dirname "$out")"

in_dev() { (cd "$repo" && nix develop -c "$@"); }

tedi_bin=${TEDI_BIN:-$repo/target/debug/tedi}
if [ ! -x "$tedi_bin" ] || [ -n "${E2E_REBUILD:-}" ]; then in_dev cargo b -p tedi >&2; fi
if [ ! -d "$here/.bundle/public" ] || [ -n "${E2E_REBUILD:-}" ]; then
  in_dev dx bundle --web -p tedi_web --out-dir "$here/.bundle" >&2
fi

alive() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }
if [ -n "${E2E_RESEED:-}" ] || [ ! -d "$XDG_DATA_HOME/tedi/issues/service-arb" ]; then
  alive && kill "$(cat "$pidfile")"
  TEDI_BIN=$tedi_bin "$here/seed.sh"
fi
if ! alive || [ -n "${E2E_REBUILD:-}" ]; then
  alive && kill "$(cat "$pidfile")" && sleep 1
  "$tedi_bin" --mock --config "$here/config.toml" web --addr 127.0.0.1:$port --public "$here/.bundle/public" >"$E2E_HOME/server.log" 2>&1 &
  echo $! >"$pidfile"
  for _ in $(seq 50); do curl -sf "http://127.0.0.1:$port/" >/dev/null && break; sleep 0.2; done
fi

# the viewer's library is the instance's GitHub user's: mock_user
mkdir -p "$XDG_DATA_HOME/tedi/web"
cp "$here/libraries/$screen.json" "$XDG_DATA_HOME/tedi/web/mock_user.json"

case $screen in 06*) size=390,844 ;; *) size=1440,900 ;; esac
chromium --headless=new --no-sandbox --disable-gpu --hide-scrollbars --force-device-scale-factor=1 \
  --user-data-dir="$E2E_HOME/chromium" --window-size=$size --virtual-time-budget=${E2E_BUDGET:-15000} \
  --screenshot="$out" "http://127.0.0.1:$port/" >/dev/null 2>&1
echo "$out"
