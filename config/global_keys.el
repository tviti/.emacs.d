;; Global keybindings
(global-set-key (kbd "\C-c ;") #'comment-region)
(local-set-key (kbd "\C-u \C-c ;") #'uncomment-region)

;; Suggested flyspell keybindings, per
;; https://www.emacswiki.org/emacs/FlySpell. Note that this is not ALL of the
;; suggested keybindings, just the ones that I "liked".
(global-set-key (kbd "<f8>") #'ispell-word)
(global-set-key (kbd "C-M-<f8>") #'flyspell-buffer)

;;;;;;;;;;;;;;;;;;;;;
;; <fn> key-chords ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key (kbd "H-m s") #'magit-status)
(global-set-key (kbd "H-m H-s m") #'magit-stage-modified)

(require 'org)
(global-set-key (kbd "H-o a") #'org-agenda)
(global-set-key (kbd "H-o H-a w") #'org-agenda-week-view)
(global-set-key (kbd "H-o H-a m") #'org-agenda-month-view)
(global-set-key (kbd "H-o H-a y") #'org-agenda-year-view)

(require 'dired)
(global-set-key (kbd "H-d c") #'dired-do-copy)
(global-set-key (kbd "H-d r") #'dired-do-rename)
