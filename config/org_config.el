(require 'org)
;; (require 'ox-bibtex) ; For bibtex citations

;; (add-hook 'org-babel-after-execute-hook
;; 	  (lambda () (org-redisplay-inline-images)))

;; Force org-babel to use python3
(setq org-babel-python-command "python3")

;; Make org-babel and elpy play nicely together see
;; https://necromuralist.github.io/posts/org-babel-ipython-and-elpy-conflict/
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq python-shell-interpreter "ipython"
		  python-shell-interpreter-args "-i --simple-prompt")))

;; Make certain languages available in org-babel code-blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

;; Get syntax highlighting in code blocks
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-edit-src-content-indentation 0)
