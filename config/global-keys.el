(require 'user-functions)

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
(global-set-key (kbd "C-c m S") #'magit-stage-file)
(global-set-key (kbd "C-c m C-s m") #'magit-stage-modified)
(global-set-key (kbd "C-c m l") #'magit-log-buffer-file)
(global-set-key (kbd "C-c m p") #'magit-push)
;; TODO: These are local, not global keybindings!
(define-key magit-mode-map (kbd "C-c m r") #'magit-diff-toggle-refine-hunk)
(with-eval-after-load 'eyebrowse
  (define-key magit-mode-map eyebrowse-keymap-prefix nil)) ;; Conflicts w/ eyebrowse

(require 'magit-annex)
(global-set-key (kbd "C-c m a") #'magit-annex-dispatch)

;; org-mode keybindings
(require 'org)
(require 'counsel)

(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'counsel-org-capture)
(global-set-key (kbd "C-c o S") #'org-save-all-org-buffers)
(global-set-key (kbd "C-c O") #'org-capture) ;; the "minimum-typing" variant
(global-set-key (kbd "C-c o r") #'org-refile)
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o C-a d") #'org-agenda-day-view)
(global-set-key (kbd "C-c o C-a w") #'org-agenda-week-view)
(global-set-key (kbd "C-c o C-a m") #'org-agenda-month-view)
(global-set-key (kbd "C-c o C-a y") #'org-agenda-year-view)
(global-set-key (kbd "C-c o f") #'org-cycle-agenda-files)

(global-set-key (kbd "C-c o o") #'org-clock-out)
(global-set-key (kbd "C-c o i") #'org-clock-in-last)
(global-set-key (kbd "C-c o g") #'org-clock-goto)
(global-set-key (kbd "C-c o z") #'org-resolve-clocks)
(global-set-key (kbd "C-c o j") #'counsel-org-goto-all)

(require 'org-ql)
(global-set-key (kbd "C-c o s") #'org-ql-search)
(global-set-key (kbd "C-c o v") #'org-ql-view)

;; counsel keybindings
(global-set-key (kbd "C-c C-r") #'ivy-resume)
(global-set-key (kbd "C-x B") #'counsel-switch-buffer)
(global-set-key (kbd "C-x C-M-b") #'counsel-ibuffer)
(global-set-key (kbd "C-c c f") #'counsel-git)
(global-set-key (kbd "C-c c g") #'counsel-git-grep)
(global-set-key (kbd "C-c c G") #'counsel-grep)
(global-set-key (kbd "C-c c o") #'counsel-outline)
(global-set-key (kbd "C-c c l") #'counsel-locate)
(global-set-key (kbd "C-c c v") #'ivy-push-view)
(global-set-key (kbd "C-c c V") #'ivy-pop-view)
(global-set-key (kbd "C-c c k") #'counsel-kmacro)
(global-set-key (kbd "C-c c r") #'counsel-register)
(global-set-key (kbd "C-c c p") #'counsel-list-processes)
(with-eval-after-load 'evil
  (global-set-key (kbd "C-c c e") #'counsel-evil-marks)
  (evil-global-set-key 'normal (kbd "M-\"") #'counsel-evil-registers))

;; nix-mode keybindings
(require 'nix)
(global-set-key (kbd "C-c n u") #'nix-shell-unpack)
(global-set-key (kbd "C-c n c") #'nix-shell-configure)
(global-set-key (kbd "C-c n b") #'nix-shell-build)
(global-set-key (kbd "C-c n e") #'nix-eshell)
(global-set-key (kbd "C-c n s") #'nix-shell)

;; misc keybindings
(global-set-key (kbd "C-c W") #'whitespace-mode)
(global-set-key (kbd "C-c i") #'imenu)
(global-set-key (kbd "C-c e") #'eshell)

(global-set-key (kbd "C-c d Y") #'tviti/copy-buffer-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hawaiian language layer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't forget that you can also very effectively insert a kahakō by using the
;; TeX input method (i.e. "C-x <ret> C-\ TeX"), although this doesn't give the
;; ʻokina.
(global-set-key (kbd "C-c h '") (lambda () (interactive) (insert ?ʻ)))

;; Lowercase kahakou chars
(global-set-key (kbd "C-c h a") (lambda () (interactive) (insert ?ā)))
(global-set-key (kbd "C-c h e") (lambda () (interactive) (insert ?ē)))
(global-set-key (kbd "C-c h i") (lambda () (interactive) (insert ?ī)))
(global-set-key (kbd "C-c h o") (lambda () (interactive) (insert ?ō)))
(global-set-key (kbd "C-c h u") (lambda () (interactive) (insert ?ū)))

;; Uppercase kahakou chars
(global-set-key (kbd "C-c h A") (lambda () (interactive) (insert ?Ā)))
(global-set-key (kbd "C-c h E") (lambda () (interactive) (insert ?Ē)))
(global-set-key (kbd "C-c h I") (lambda () (interactive) (insert ?Ī)))
(global-set-key (kbd "C-c h O") (lambda () (interactive) (insert ?Ō)))
(global-set-key (kbd "C-c h U") (lambda () (interactive) (insert ?Ū)))

;; "Free up" conflicts between Shell-mode and eyebrowse
;; TODO: These are local, not GLOBAL keybindings!
(with-eval-after-load 'eyebrowse
  (define-key shell-mode-map eyebrowse-keymap-prefix nil))

(global-set-key (kbd "C-c w") #'tviti/evil-window)

(provide 'global-keys)
