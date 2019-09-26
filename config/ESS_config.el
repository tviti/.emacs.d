(require 'ess-site)
;; Rstudio style rules
(add-hook 'ess-mode-hook
	  (lambda () (ess-set-style 'RStudio)))
