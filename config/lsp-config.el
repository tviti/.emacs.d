;;
;; Eglot config
;;
(require 'eglot)
(add-hook 'ess-r-mode-hook #'eglot-ensure)
(add-hook 'tex-mode-hook #'eglot-ensure)
(add-hook 'bibtex-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs  '(tex-mode "texlab"))
(add-to-list 'eglot-server-programs  '(bibtex-mode "texlab"))

(require 'linter-config)
(defun tviti/setup-latex-lsp ()
  (when (eq tviti/linter 'flymake)
    ;; Retain the default latex flymake linter
    (setq-local eglot-stay-out-of '(flymake))
    (setq-local flymake-diagnostic-functions '(LaTeX-flymake eglot-flymake-backend t)))
  ;; Force auctex to use eglot completion candidates
  (define-key TeX-mode-map (kbd "C-M-i") #'complete-symbol))

(with-eval-after-load 'tex
  (add-hook 'TeX-mode-hook #'tviti/setup-latex-lsp))

(provide 'lsp-config)
