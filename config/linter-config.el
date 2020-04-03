(require 'cl-lib)

;; Use flyspell mode in text-mode buffers (e.g. org-mode), but NOT in
;; change-log-mode or log-edit-mode. Taken from
;; https://www.emacswiki.org/emacs/Fly Spell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(defvar tviti/linter 'flymake
  "Which linter to use.  Should be one of 'flycheck or 'flymake.")

(cl-defun tviti/linter-on (&key (linter tviti/linter)
				(prog-mode-p t))
  "Enable linting in the active buffer. LINTER is one of
'flycheck or 'flymake (default value is `tviti/linter'). Use
PROG-MODE-P to turn on/off flyspell `prog-mode'."
  (cond ((eq linter 'flycheck)
	 (flycheck-mode 1))
	((eq linter 'flymake)
	 (flymake-mode 1)))
  (when prog-mode-p (flyspell-prog-mode)))


;; modes to lint
;; (setq ess-use-flymake nil)
(add-hook 'ess-r-mode-hook #'tviti/linter-on)
;; The flymake checkdoc backend seems to be broken, so we shut it off (note that
;; this seems to work OK in flycheck, but is still kind of annoying since
;; checkdoc is a little aggressive.
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (tviti/linter-on)
				  (remove-hook 'flymake-diagnostic-functions
					       'elisp-flymake-checkdoc t)))
(add-hook 'LaTeX-mode-hook (lambda () (tviti/linter-on :prog-mode-p nil)))

(with-eval-after-load 'nix-mode
  (require 'nix-flymake)
  (add-hook 'nix-mode-hook (lambda ()
			     (add-hook 'flymake-diagnostic-functions #'nix-flymake nil t)
			     (tviti/linter-on))))

;; Don't let flyspell take control of "C-M-i"
(define-key flyspell-mode-map (kbd "C-M-i") nil)

(provide 'linter-config)
