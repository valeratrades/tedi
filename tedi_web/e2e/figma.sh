#!/usr/bin/env bash
# figma.sh [screen...]   Figma's reference png of each screen frame into reference/ (Figma REST API; needs FIGMA_TOKEN).
# Without a token: ask Figma's MCP get_screenshot for the node ids below (maxDimension 2880) and curl the returned image_url.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
file=AKNYVRnUHnGrnoFN6geWii
declare -A node=([01-board]=48:5386 [02-view]=10:1607 [03-library]=11:3139 [04-workspace]=12:3114 [05-scope]=48:6437 [06a-mobile]=13:5217 [06b-mobile]=13:5488 [07-first-open]=48:7379 [08-none-open]=48:7797)
screens=("$@"); [ ${#screens[@]} -gt 0 ] || screens=(${!node[@]})
: "${FIGMA_TOKEN:?set FIGMA_TOKEN, or fetch via the MCP as described above}"
mkdir -p "$here/reference"
for s in "${screens[@]}"; do
  url=$(curl -sf -H "X-Figma-Token: $FIGMA_TOKEN" "https://api.figma.com/v1/images/$file?ids=${node[$s]}&format=png&scale=1" | jq -r ".images[\"${node[$s]}\"]")
  curl -sf -L -o "$here/reference/$s.png" "$url"
  echo "$here/reference/$s.png"
done
