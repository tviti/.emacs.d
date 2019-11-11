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

;; magit keybindings. Don't forget about "C-c M-g" as well!
(require 'magit)
(global-set-key (kbd "C-c m s") #'magit-status)
(global-set-key (kbd "C-c m C-s m") #'magit-stage-modified)
(global-set-key (kbd "C-c m l") #'magit-log-buffer-file)

(require 'magit-annex)
(global-set-key (kbd "C-c m a") #'magit-annex-dispatch)

;; org-mode keybindings
(require 'org)
(require 'counsel)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'counsel-org-capture)
(global-set-key (kbd "C-c O") #'org-capture) ;; the "minimum-typing" variant
(global-set-key (kbd "C-c o r") #'org-refile)
(global-set-key (kbd "C-c o C-a d") #'org-agenda-day-view)
(global-set-key (kbd "C-c o C-a w") #'org-agenda-week-view)
(global-set-key (kbd "C-c o C-a m") #'org-agenda-month-view)
(global-set-key (kbd "C-c o C-a y") #'org-agenda-year-view)

(global-set-key (kbd "C-c o o") #'org-clock-out)
(global-set-key (kbd "C-c o i") #'org-clock-in-last)

(global-set-key (kbd "C-c o n") #'org-next-block)
(global-set-key (kbd "C-c o p") #'org-previous-block)

(global-set-key (kbd "C-c o j") #'counsel-org-goto-all)

;; counsel keybindings
(global-set-key (kbd "C-c C-r") #'ivy-resume)
(global-set-key (kbd "C-c c g") #'counsel-git-grep)
(global-set-key (kbd "C-c c o") #'counsel-outline)
(global-set-key (kbd "C-c c l") #'counsel-locate)
(global-set-key (kbd "C-c c v") #'ivy-push-view)
(global-set-key (kbd "C-c c V") #'ivy-pop-view)
(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "M-\"") #'counsel-evil-registers))

(require 'dired)
;; TODO: These were made before I realized that there were builtins.
(global-set-key (kbd "C-c d c") #'dired-do-copy)
(global-set-key (kbd "C-c d r") #'dired-do-rename)

;; misc keybindings
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c i") #'imenu)

(with-eval-after-load 'flymake
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

(provide 'global-keys)
