# Integration Tests

## Key Invariants

1. **Tests should not know implementation details.** The test infrastructure in `common/` provides abstractions over git, mock GitHub, etc. Tests should only use these abstractions - they should treat the system as a black box that can:
   - Save an `Issue`
   - Show a currently saved `Issue`

2. **Use `Issue` as the canonical representation.** All test setup should work with `Issue` structs. The test infrastructure handles translation to/from mock GitHub API format, git state, etc.

3. **No raw JSON in tests.** If you need new mock capabilities (like `state_reason` for duplicates), extend the infrastructure in `common/`, don't write raw JSON in test files.
