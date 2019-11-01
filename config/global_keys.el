;; Global keybindings
(global-set-key (kbd "\C-c ;") #'comment-region)
(local-set-key (kbd "\C-u \C-c ;") #'uncomment-region)

;; Suggested flyspell keybindings, per
;; https://www.emacswiki.org/emacs/FlySpell. Note that this is not ALL of the
;; suggested keybindings, just the ones that I "liked".
(global-set-key (kbd "<f8>") #'ispell-word)
(global-set-key (kbd "C-M-<f8>") #'flyspell-buffer)

;; Default to using ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;
;; Custom key-chords ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key (kbd "C-c m s") #'magit-status)
(global-set-key (kbd "C-c m C-s m") #'magit-stage-modified)

(require 'org)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o r") #'org-refile)
(global-set-key (kbd "C-c o C-a d") #'org-agenda-day-view)
(global-set-key (kbd "C-c o C-a w") #'org-agenda-week-view)
(global-set-key (kbd "C-c o C-a m") #'org-agenda-month-view)
(global-set-key (kbd "C-c o C-a y") #'org-agenda-year-view)

;; (global-set-key (kbd "C-c o o") #'org-insert-heading-after-current)
(global-set-key (kbd "C-c o o") #'org-clock-out)
(global-set-key (kbd "C-c o i") #'org-clock-in-last)

(global-set-key (kbd "C-c o n") #'org-next-block)
(global-set-key (kbd "C-c o p") #'org-previous-block)

(require 'counsel)
(global-set-key (kbd "C-c o j") #'counsel-org-goto-all)

(require 'dired)
;; TODO: These were made before I realized that there were builtins.
(global-set-key (kbd "C-c d c") #'dired-do-copy)
(global-set-key (kbd "C-c d r") #'dired-do-rename)

(global-set-key (kbd "C-c i") #'imenu)
