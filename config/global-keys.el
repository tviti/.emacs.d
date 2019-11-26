;; Global keybindings
(global-set-key (kbd "\C-c ;") #'comment-region)
(local-set-key (kbd "\C-u \C-c ;") #'uncomment-region)

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
(global-set-key (kbd "C-c m p") #'magit-push)
;; TODO: These are local, not global keybindings!
(define-key magit-mode-map (kbd "C-c m r") #'magit-diff-toggle-refine-hunk)
(define-key magit-mode-map (kbd "C-c C-w") 'nil) ;; Conflicts w/ eyebrowse

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

;; TODO: These are local, not global keybindings!
(define-key org-mode-map (kbd "C-c o n") #'org-next-block)
(define-key org-mode-map (kbd "C-c o p") #'org-previous-block)

(global-set-key (kbd "C-c o j") #'counsel-org-goto-all)

;; counsel keybindings
(global-set-key (kbd "C-c C-r") #'ivy-resume)
(global-set-key (kbd "C-x B") #'counsel-switch-buffer)
(global-set-key (kbd "C-c c f") #'counsel-git)
(global-set-key (kbd "C-c c g") #'counsel-git-grep)
(global-set-key (kbd "C-c c o") #'counsel-outline)
(global-set-key (kbd "C-c c l") #'counsel-locate)
(global-set-key (kbd "C-c c v") #'ivy-push-view)
(global-set-key (kbd "C-c c V") #'ivy-pop-view)
(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "M-\"") #'counsel-evil-registers))

;; misc keybindings
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c i") #'imenu)

(provide 'global-keys)
