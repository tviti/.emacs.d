;;
;; Eglot config
;;
(require 'eglot)
(add-hook 'ess-r-mode-hook #'eglot-ensure)
(add-hook 'tex-mode-hook #'eglot-ensure)
(add-hook 'bibtex-mode-hook #'eglot-ensure)
(add-hook 'python-mode #'eglot-ensure)
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
  (define-key TeX-mode-map (kbd "C-M-i") #'complete-symbol)
  (define-key TeX-mode-map (kbd "C-c f C-c") #'tviti/eglot-texlab-build))

(with-eval-after-load 'tex
  (add-hook 'TeX-mode-hook #'tviti/setup-latex-lsp))

;;
;; Eglot support in org-babel. Based on
;; https://github.com/seagle0128/.emacs.d/blob/03a69b9b725a228a451ec27d7506a2d24f4d1a0f/lisp/init-lsp.el#L436
;;
(cl-defmacro tviti/lsp-org-babel-enable (lang)
  "Support LANG in org source code block. `eglot' will be
activated in the `org-edit-special' buffers of blocks that have a
#+name: specified. The blocks #+name: must translate into an
acceptable filename string, complete with file extension."
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
	 (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
	 (let ((name (nth 4 info)))
	   (unless name
	     (user-error "LSP:: specify block #+name: to enable"))

	   (setq buffer-file-name name)
	   (eglot-ensure)))
	   ;; (execute-extended-command nil "eglot")))
       (put ',intern-pre 'function-documentation
	    (format "Enable `%s' in the buffer of org source block (%s)."
		    'eglot (upcase ,lang)))

       (if (fboundp ',edit-pre)
	   (advice-add ',edit-pre :after ',intern-pre)
	 (progn
	   (defun ,edit-pre (info)
	     (,intern-pre info))
	   (put ',edit-pre 'function-documentation
		(format "Prepare local buffer environment for org source block (%s)."
			(upcase ,lang))))))))

;; (tviti/lsp-org-babel-enable "python")

;;
;; Misc. keybindings
;;
(define-transient-command tviti/linter-nav ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Errors"
    ("p" "previous message" flymake-goto-prev-error)
    ("n" "next message" flymake-goto-next-error)]
   ["Formatting"
    ("r" "rename" eglot-rename)
    ("f" "format" eglot-format)
    ("F" "format buffer" eglot-format-buffer)]
   ["xref"
    ("x" "find refs" xref-find-references)
    ("d" "find defs" xref-find-definitions)]
   ["Help"
    ("h" "eglot help" eglot-help-at-point)]])

(define-key eglot-mode-map (kbd "C-c f") #'tviti/linter-nav)

(provide 'lsp-config)
