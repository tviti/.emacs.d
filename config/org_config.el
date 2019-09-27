(require 'org)
;; (require 'ox-bibtex) ; For bibtex citations

;; (add-hook 'org-babel-after-execute-hook
;; 	  (lambda () (org-redisplay-inline-images)))

;; Force org-babel to use python3
;; (setq org-babel-python-command "python3")

;; Make org-babel and elpy play nicely together see
;; https://necromuralist.github.io/posts/org-babel-ipython-and-elpy-conflict/
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (setq python-shell-interpreter "ipython"
;; 		  python-shell-interpreter-args "-i --simple-prompt")))

;; Experimental: julia code blocks via the abandoned ob-julia.el
;; (setq inferior-julia-program-name "/usr/local/bin/julia")
;; (load-file (concat (file-name-directory load-file-name) "./ob-julia.el"))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;; Get syntax highlighting in code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; Make sure markdown shows up in the export menu
(require 'ox-md nil t)

;; Load agenda file lists
(if (string-equal (system-name) "R-Daneel.local")
    (setq org-agenda-files "~/.emacs.d/R-Daneel-agenda-files.txt"))
(if (string-equal (system-name) "magneto")
    (setq org-agenda-files "~/.emacs.d/magneto-agenda-files.txt"))
