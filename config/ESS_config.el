(require 'ess-site)
;; Rstudio style rules
(add-hook 'ess-mode-hook
	  (lambda () (ess-set-style 'RStudio)))

;; Use polymode for working with .Rmd files
(require 'polymode)
(require 'poly-markdown)
(require 'poly-R)
