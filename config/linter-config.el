(require 'cl-lib)

;; Use flyspell mode in text-mode buffers (e.g. org-mode), but NOT in
;; change-log-mode or log-edit-mode. Taken from
;; https://www.emacswiki.org/emacs/FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(defvar tviti/linter 'flycheck
  "Which linter to use.  Should be one of 'flycheck or 'flymake.")

(cl-defun tviti/linter-on (&optional (linter tviti/linter))
  "Enable linting in the active buffer."
  (cond ((eq linter 'flycheck)
	 (flycheck-mode 1))
	((eq linter 'flymake)
	 (flymake-mode 1)))
  (flyspell-prog-mode))


;; modes to lint
(add-hook 'emacs-lisp-mode-hook #'tviti/linter-on)
(add-hook 'LaTeX-mode-hook #'tviti/linter-on)
(setq ess-use-flymake nil)
(add-hook 'ess-r-mode-hook #'tviti/linter-on)

(provide 'linter-config)
