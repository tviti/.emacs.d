(require 'julia-mode)
(push "~/lsp-julia/" load-path)
;; Various BS trying to get my dev modules recognized
;; (setq lsp-julia-package-dir 'nil)
;; (setq lsp-julia-package-dir "/Users/taylor/julia-vscode-0.12.2/scripts/languageserver/packages")

;; (setq lsp-julia-default-environment "/Users/taylor/BRAAP.jl")
;; (setenv "JULIA_DEPOT_PATH" "/Users/taylor/BRAAP.jl/src")
(require 'lsp-julia)
(require 'lsp-mode)

;; Configure lsp + flycheck via lsp-ui
(require 'flycheck)
(global-flycheck-mode)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'julia-mode-hook 'flycheck-mode)
(setq lsp-prefer-flymake nil)

;; Configure lsp + julia
(add-hook 'julia-mode-hook #'lsp-mode)
(add-hook 'julia-mode-hook #'lsp)

;; Don't bomb the user with object documentation
(setq lsp-ui-doc-enable 'nil)
(setq lsp-ui-sideline-enable 'nil)

;; Keybindings
(define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)

;; Override xref keybindings w/ lsp-ui-peek
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; (define-key lsp-ui-mode-map (kbd "\C-c d") #'lsp-ui-doc-show)
;; (define-key lsp-ui-mode-map (kbd "\C-u \C-c d") #'lsp-ui-doc-hide)

(setq lsp-eldoc-enable-hover 't)
(setq lsp-eldoc-render-all 'nil)
(setq lsp-eldoc-prefer-signature-help 't)

;; (setq lsp-log-io 't)
(setq lsp-log-io 'ninill)
;; (setq lsp-signature-render-all 'nil)
