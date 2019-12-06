(require 'ess-site)

;; Rstudio style rules
(add-hook 'ess-mode-hook
	  (lambda () (ess-set-style 'RStudio)))

;; Use polymode for working with .Rmd files
(require 'polymode)
(require 'poly-markdown)
(require 'poly-R)

;; Re-def to 'nil so that polymode doesn't open export results.
(defun pm--display-file (ofile)
  "An empty defun to placate and quiet `polymode' (arg OFILE is unused)."
  '())

;; Turn on line-numbering in markdown-mode so that we can get line numbers when
;; editing .Rmd files
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (display-line-numbers-mode)
	    (setq display-line-numbers 'relative)))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Don't use ido for ESS completions. Disabling this allows ESS to use other
;; completion backends, like ivy.
(setq ess-use-ido nil)
		 

(provide 'ess-config)
