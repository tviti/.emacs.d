;; Completions via the package emacs-bash-completion.  This snippet was taken
;; from https://github.com/szermatt/emacs-bash-completion/issues/13
(require 'eshell)
(require 'pcomplete)
(use-package bash-completion)

(setq eshell-default-completion-function 'eshell-bash-completion)

(defun eshell-bash-completion ()
  (while (pcomplete-here
          (nth 2 (bash-completion-dynamic-complete-nocomint
		  (save-excursion (eshell-bol) (point)) (point))))))

;; eshell-mode-map is buffer local and nil outside of eshell-mode, so we have to
;; delay calls to define-key.
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)))

(provide 'eshell-config)
