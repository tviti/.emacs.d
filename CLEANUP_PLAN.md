# Repository Cleanup & Maintenance Plan

**Goal:** Minimize entropy, reduce dead code, and implement a professional secret management framework while maintaining portability.

## Phase 1: Audit & Mapping (Read-Only)
- [ ] Dependency Matrix: Map `use-package` declarations vs `(require ...)` calls.
- [ ] Entropy Scan: Analyze `config/*.el` for duplicated logic and redundant hooks.
- [ ] Dead Code Inventory: List commented-out blocks and empty directories for removal.

## Phase 2: Structural Reorganization
- [ ] The Package Manifest: Move `use-package` declarations to `config/packages.el`.
- [ ] Entry-Point Refactoring: Slim down `init.el` into a high-level orchestrator.
- [ ] Standardization: Ensure `config/` files adhere to `README.md` conventions.

## Phase 3: Logic Deduplication & Pruning
- [ ] Centralized Utilities: Move repeated snippets to `user-functions.el` or `ui-config.el`.
- [ ] The Great Pruning: Remove dead code and noise.
- [ ] Variant Consolidation: Migrate hard-coded paths and OS hacks to `user-globals.el` or `macos-config.el`.

## Phase 4: Secret Management Framework
- [ ] Provider Abstraction: Implement `get-secret` utility (Keychain -> GPG -> Env).
- [ ] Secure Loading Logic: Create `load-secret-config` wrapper for in-memory decryption.
- [ ] Verification: Run `make test` and `make bootstrap` to ensure no regressions.
