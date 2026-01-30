(require 'user-globals)
(require 'org)
(require 'org-crypt)

(setq org-directory "~/org")
(setq org-agenda-files (list org-directory))

;; org-crypt setup (based on [[info:org#Org Crypt][info:org#Org Crypt]])
(org-crypt-use-before-save-magic)
(setq org-crypt-key nil)
(setq org-tags-exclude-from-inheritance '("crypt"))

(use-package org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Enable inline todo items
(require 'org-inlinetask)

(use-package ox-gfm)

(use-package htmlize)

;; Configure the languages for source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((eshell . t)
   (shell . t)
   (R . t)
   (org . t)))

;; Configure the export backends
;; (eval-after-load "org"
;;   (progn
;;     '(require 'ox-gfm nil t)))
;;(with-eval-after-load 'org
;;  (use-package ox-gfm))
(add-to-list 'org-export-backends 'md)

;; (require 'ox-bibtex) ; For bibtex citations

(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-redisplay-inline-images)))

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

;; (setq org-agenda-fles "~/org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appointments and reminders ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(appt-activate)

;; Automatically create timers when calling `org-agenda'
(add-hook 'org-agenda-mode-hook
	  (lambda () (org-agenda-to-appt)))

;; Define a notification function for creating GUI notifications
(if (string= system-type "darwin")
    (when (tviti/mac-port-p) ;; mac-osa-script is mac-port specific
    (setq appt-disp-window-function
	  (lambda (min-to-app new-time appt-msg)
	    ;; Retain original functionality
	    (appt-disp-window min-to-app new-time appt-msg)
	    ;; Create a native macOS desktop notification via applescript
	    (let ((title "SOMETHING IS HAPPENING!?!?")
		  (sound-name "Ring"))
	      (mac-osa-script
	       (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
	      	       appt-msg title sound-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; org-capture setup ;;
;;;;;;;;;;;;;;;;;;;;;;;
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-refile-targets `((nil . (:maxlevel . 3))
			   (,(org-agenda-files t) . (:maxlevel . 3))))

;; Load the capture templates
(use-package doct)

(defun tviti/org-insert-time-range (dt)
  "Returns an active time range at point from now till DT minutes."
  (let* ((date-input (org-read-date t t))
	 (date (format-time-string (cdr org-time-stamp-formats) date-input))
         ;; (shifted (concat (substring date 0 -1)
	 ;; 		  (format " +%sm" dt)
	 ;; 		  (substring date -1))))
	 (shifted (format-time-string (cdr org-time-stamp-formats)
				      (time-add date-input (seconds-to-time (* dt 60))))))
    (format "%s--%s" date shifted)))

;(let ((fn (concat org-directory "/capture-templates.el")))
; (with-temp-buffer
;   (insert-file-contents-literally fn)
;   (setq org-capture-templates (doct (eval (read (buffer-string)))))))

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
	     '("lf" . "LaTeX latex"))

;; Latex fragment w/ un-numbered equation
(add-to-list 'org-structure-template-alist
	     '("lfe" . "LaTeX latex
\\begin{equation}
\\end{equation}"))

(add-to-list 'org-structure-template-alist
	     '("lfE" . "LaTeX latex
\\begin{equation*}
\\end{equation*}"))

;; Flyspell will take over the standard completion keybindings, so we set them
;; ourselves here
(define-key org-mode-map (kbd "M-<tab>")
  (lambda () (interactive) (pcomplete-std-complete)))

(define-key org-mode-map (kbd "C-M-i")
  (lambda () (interactive) (pcomplete-std-complete)))

(provide 'org-config)
