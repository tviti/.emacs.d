(require 'org)

;; Enable inline todo items
(require 'org-inlinetask)

;; Configure the languages for source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

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

;;;;;;;;;;;;;;;;;;;;;;;
;; org-capture setup ;;
;;;;;;;;;;;;;;;;;;;;;;;

(when tviti/sync-dir
  (setq org-directory (concat tviti/sync-dir "/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org")))

(setq org-refile-targets `((nil . (:maxlevel . 3))
			   (,(org-agenda-files t) . (:maxlevel . 3))))

;; Load the capture templates
(let ((fn (concat org-directory "/capture-templates.el")))
  (with-temp-buffer
    (insert-file-contents-literally fn)
    (setq org-capture-templates (read (buffer-string)))))

;; WORKAROUND:Incremental refile completion doesn't work with ivy-mode (see
;; https://github.com/abo-abo/swiper/issues/1254 and
;; https://github.com/abo-abo/swiper/issues/444), so if we want to refile to an
;; actual sub tree path, it has to be in one go.
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom easy templates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
