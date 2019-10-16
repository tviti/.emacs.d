(require 'org)

;; Configure the export backends
(eval-after-load "org"
  (progn
    '(require 'ox-md nil t)
    '(require 'ox-gfm nil t)))

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

;; Load agenda file lists
(cond
 ((string-equal (system-name) "R-Daneel.local")
  (setq org-agenda-files "~/.emacs.d/R-Daneel-agenda-files.txt"))
 ((string-equal (system-name) "magneto")
  (setq org-agenda-files "~/.emacs.d/magneto-agenda-files.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appointments and reminders ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically create timers when calling `org-agenda'
(add-hook 'org-agenda-mode-hook
	  (lambda () (org-agenda-to-appt)))

;; Define a notification function for creating GUI notifications
(if (string= system-type "darwin")
    (setq appt-disp-window-function
	  (lambda (min-to-app new-time appt-msg)
	    ;; Retain original functionality
	    (appt-disp-window min-to-app new-time appt-msg)
	    ;; Create a native macOS desktop notification via applescript
	    (let ((title "SOMETHING IS HAPPENING!?!?")
		  (sound-name "Ring"))
	      (shell-command
	       (format "osascript -e 'display notification \"%s\" with title \"%s\" sound name \"%s\"'"
		       appt-msg title sound-name))))))

;; Custom easy templates
;; Latex "fragments"
(add-to-list 'org-structure-template-alist
	     '("lf" "#+BEGIN_LaTeX latex
?
#+END_LaTeX"))

;; Latex fragment w/ un-numbered equation
(add-to-list 'org-structure-template-alist
	     '("lfe" "#+BEGIN_LaTeX latex
\\begin{equation}
?
\\end{equation}
#+END_LaTeX"))

(add-to-list 'org-structure-template-alist
	     '("lfE" "#+BEGIN_LaTeX latex
\\begin{equation*}
?
\\end{equation*}
#+END_LaTeX"))



