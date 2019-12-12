;;
;; Eglot config
;;
(require 'eglot)
(add-hook 'ess-r-mode-hook #'eglot-ensure)
(add-hook 'tex-mode-hook #'eglot-ensure)
(add-hook 'bibtex-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs  '((tex-mode bibtex-mode latex-mode yatex-mode) "texlab"))

(require 'linter-config)
(require 'tex) ;; auctex

(defun tviti/eglot-texlab-build ()
  "Call the method textDocument/build on `TeX-master'."
  (interactive)
  (lexical-let ((fname (if (eq TeX-master t)
			(buffer-file-name)
			(file-truename (format "%s.tex" TeX-master))))
		(docname (expand-file-name (TeX-active-master (TeX-output-extension)))))
    (jsonrpc-async-request (eglot--current-server-or-lose) :textDocument/build
			   `(:textDocument
			     (:uri ,(eglot--path-to-uri fname)))
			   :success-fn
			   (lambda (result)
			     (let ((status (plist-get result :status)))
			       (if (eq status 0)
				   (progn
				     (princ (format "Compilation successful! Reverting buffer for %s." docname))
				     (TeX-revert-document-buffer docname))
				   (princ (format "Compilation failed with status %s." status))))))))

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
