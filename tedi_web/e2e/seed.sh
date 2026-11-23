#!/usr/bin/env bash
# Builds the seeded store under $E2E_HOME (default: a fresh temp dir) and prints nothing; env for the server comes from env.sh.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
. "$here/env.sh"
root=$E2E_HOME
rm -rf "$root"
mkdir -p "$root"
cp "$here/seed/state.json" "$tedi_MOCK_STATE"
mkdir -p "$XDG_DATA_HOME/tedi/issues" "$XDG_STATE_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME/tedi/web"
git -C "$XDG_DATA_HOME/tedi/issues" init -q
git -C "$XDG_DATA_HOME/tedi/issues" config user.name e2e
git -C "$XDG_DATA_HOME/tedi/issues" config user.email e2e@localhost

tedi_bin=${TEDI_BIN:-tedi}
$tedi_bin --mock --config "$here/config.toml" pull >&2

# the one unreadable issue the sidebar footer counts
mkdir -p "$XDG_DATA_HOME/tedi/issues/ev-main/gitops"
printf 'not an issue\n' > "$XDG_DATA_HOME/tedi/issues/ev-main/gitops/3_-_broken.md"

# tedi's own state (kanban positions, label colours) is not on the GitHub mock: written beside the pulled issues.
positions="$here/seed/positions.json"
for repo in $(jq -r '.kanban + .pull_requests | keys[]' "$positions"); do
  dir="$XDG_DATA_HOME/tedi/issues/$repo"
  [ -d "$dir" ] || { echo "pull stored nothing for $repo" >&2; exit 1; }
  jq --arg r "$repo" '{kanban: (.kanban[$r] // {}), pull_requests: (.pull_requests[$r] // {})} | with_entries(select(.value != {}))' "$positions" > "$dir/.states.json"
  meta="$dir/.meta.json"
  [ -f "$meta" ] || meta=$(ls "$dir"/.*meta*.json | head -1)
  jq --slurpfile p "$positions" '.label_colors = ($p[0].label_colors | map_values(.))' "$meta" > "$meta.tmp" && mv "$meta.tmp" "$meta"
done
