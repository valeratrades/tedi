#!/usr/bin/env bash
# shoot.sh <screen> [out.png]   screens: 01-board 02-view 03-library 04-workspace 05-scope 06a-mobile 06b-mobile
# shoot.sh stop                 stops the server it started
# env: E2E_HOME (store, default /tmp/tedi-e2e) E2E_PORT (7351) E2E_RESEED=1 E2E_REBUILD=1 (client bundle) TEDI_BIN
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
. "$here/env.sh"
port=${E2E_PORT:-7351}
[ "${1:-}" = stop ] && exec "$here/serve.sh" stop
screen=${1:?screen}
out=${2:-$here/out/$screen.png}
[ -f "$here/libraries/$screen.json" ] || { echo "no library for $screen" >&2; exit 1; }
mkdir -p "$(dirname "$out")"
"$here/serve.sh"

# the viewer's library is the instance's GitHub user's: mock_user
mkdir -p "$XDG_DATA_HOME/tedi/web"
cp "$here/libraries/$screen.json" "$XDG_DATA_HOME/tedi/web/mock_user.json"

case $screen in 06*) size=390,844 ;; *) size=1440,900 ;; esac
chromium --headless=new --no-sandbox --disable-gpu --hide-scrollbars --force-device-scale-factor=1 \
  --user-data-dir="$E2E_HOME/chromium" --window-size=$size --virtual-time-budget=${E2E_BUDGET:-15000} \
  --screenshot="$out" "http://127.0.0.1:$port/" >/dev/null 2>&1
echo "$out"
