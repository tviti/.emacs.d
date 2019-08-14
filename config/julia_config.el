(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; I don't think this helps much, but load the term-mode evil-collection
;; so that we can get "better" evil integration with julia-repl
;; (which uses term-mode)
(require 'evil-collection)
(evil-collection-init 'term)
