(require 'user-globals)
(require 'org)
(require 'org-crypt)

(setq org-directory "~/org")
(setq org-agenda-files (list org-directory))

;; org-crypt setup (based on [[info:org#Org Crypt][info:org#Org Crypt]])
(org-crypt-use-before-save-magic)
(setq org-crypt-key nil)
(setq org-tags-exclude-from-inheritance '("crypt"))

(require 'org-ql)

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Enable inline todo items
(require 'org-inlinetask)

(require 'ox-gfm)

(require 'htmlize)

;; Configure the languages for source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((eshell . t)
   (shell . t)
   (R . t)
   (org . t)))

;; Configure the export backends
(add-to-list 'org-export-backends 'md)

(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-redisplay-inline-images)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Get syntax highlighting in code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

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
(require 'doct)

(defun tviti/org-insert-time-range (dt)
  "Returns an active time range at point from now till DT minutes."
  (let* ((date-input (org-read-date t t))
         (date (format-time-string (cdr org-time-stamp-formats) date-input))
         (shifted (format-time-string (cdr org-time-stamp-formats)
                                      (time-add date-input (seconds-to-time (* dt 60))))))
     (format "%s--%s" date shifted)))


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
