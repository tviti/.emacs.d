(require 'julia-mode)
(push "~/lsp-julia/" load-path)
;; (load-file "~/lsp-julia/lsp-julia.el")
(require 'lsp-julia)
(require 'lsp-mode)

;; Configure lsp + julia
(add-hook 'julia-mode-hook #'lsp-mode)
(add-hook 'julia-mode-hook #'lsp)
