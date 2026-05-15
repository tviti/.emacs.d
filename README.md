## .emacs.d

A git repo to track my emacs config, and simplify installation/synchronization
across multiple machines.

## Conventions

### Layout

Configurations are organized into modules located in `config/`. Each module
invokes `(provide ...)` to announce a feature that is loaded by `init.el`.

Some configurations have the potential to leak private metadata, such as
hostnames and service names (e.g. for LLM APIs). The mechanism for excluding
these is to place them into a gitignored config module that `provide`s a
`private-config` feature. I normally use a directory named `private/` as a
dumping ground for these configs; this repo implements an ignore rule for this
directory, and `init.el` adds it to the `load-path`.

### Package loading

Top-level `use-package` declarations live in `init.el`. Config files that use
symbols from a package should `(require 'package-name)` at the top of the
file — this lets linters (e.g. `flymake-elisp`) resolve symbols when checking
files in isolation, and makes each file's dependencies explicit.

Since `use-package-always-ensure t` is set globally, individual `use-package`
forms should omit `:ensure t`.

## Testing

An [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/) test suite
validates the configuration to catch errors during bootstrapping.

```sh
make test-pre    # Syntax, structure, load-order (no packages needed)
make test        # Full suite including byte-compilation and load tests
make bootstrap   # Install packages then run full suite (fresh machine)
make clean       # Remove .elc files
```
