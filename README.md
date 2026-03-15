## .emacs.d

A git repo to track my emacs config, and simplify installation/synchronization
across multiple machines.

## Conventions

### Package loading

Top-level `use-package` declarations live in `init.el`. Config files that use
symbols from a package should `(require 'package-name)` at the top of the
file — this lets linters (e.g. `flymake-elisp`) resolve symbols when checking
files in isolation, and makes each file's dependencies explicit.

Since `use-package-always-ensure t` is set globally, individual `use-package`
forms should omit `:ensure t`.
