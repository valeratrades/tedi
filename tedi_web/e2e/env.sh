# Sourced by seed.sh / shoot.sh: the isolated store and the mock GitHub.
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
export E2E_HOME=${E2E_HOME:-/tmp/tedi-e2e}
export XDG_DATA_HOME=$E2E_HOME/data XDG_STATE_HOME=$E2E_HOME/state XDG_CACHE_HOME=$E2E_HOME/cache
export tedi_MOCK_STATE=$E2E_HOME/mock-state.json tedi_MOCK_NOW=2026-11-23T12:00:00Z
