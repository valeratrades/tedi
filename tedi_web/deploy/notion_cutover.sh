#!/usr/bin/env bash
# One-off, at cutover from notion_sync: the stages only Notion knew (Icebox, Selected, In progress) become
# tedi's `s:` pins on GitHub. Every other stage is read off GitHub already. Dry run unless `--apply`.
#   notion_cutover.sh path/to/notion_sync.db [--apply]
set -euo pipefail

db=${1:?the notion_sync sqlite db}
apply=${2:-}

sqlite3 -separator ' ' "$db" "
	SELECT repo, number, CASE stage WHEN 'icebox' THEN 's:icebox' WHEN 'selected' THEN 's:selected' WHEN 'in_progress' THEN 's:wip' END
	FROM issue_map
	WHERE stage IN ('icebox', 'selected', 'in_progress') AND repo IS NOT NULL AND state = 'open'
	ORDER BY repo, number" |
while read -r repo number pin; do
	if [ "$apply" = --apply ]; then
		gh issue edit "$number" --repo "$repo" --add-label "$pin"
	else
		echo "gh issue edit $number --repo $repo --add-label $pin"
	fi
done
