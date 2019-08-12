(require 'julia-mode)

(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; (flycheck-julia-setup)
