(require 'ess-site)
;; Rstudio style rules
(add-hook 'ess-mode-hook
	  (lambda () (ess-set-style 'RStudio)))

;; Use polymode for working with .Rmd files
(require 'polymode)
(require 'poly-markdown)
(require 'poly-R)

;; Turn on line-numbering in markdown-mode so that we can get line numbers when
;; editing .Rmd files
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (display-line-numbers-mode)
	    (setq display-line-numbers 'relative)))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
