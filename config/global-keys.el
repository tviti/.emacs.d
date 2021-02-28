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
(define-prefix-command 'tviti/magit-map)
(global-set-key (kbd "C-c m") #'tviti/magit-map)
(define-key 'tviti/magit-map (kbd "s") #'magit-status)
(define-key 'tviti/magit-map (kbd "f") #'magit-find-file)
(define-key 'tviti/magit-map (kbd "S") #'magit-stage-file)
(define-key 'tviti/magit-map (kbd "C-s m") #'magit-stage-modified)
(define-key 'tviti/magit-map (kbd "l") #'magit-log-buffer-file)
(define-key 'tviti/magit-map (kbd "p") #'magit-push)
(with-eval-after-load 'eyebrowse
  (define-key magit-mode-map eyebrowse-keymap-prefix nil)) ;; Conflicts w/ eyebrowse

(with-eval-after-load 'magit-annex
  (define-key 'tviti/magit-map (kbd "a") #'magit-annex-dispatch))

;; org-mode keybindings
(require 'org)
(require 'counsel)

(global-set-key (kbd "C-c O") #'org-capture)

(define-prefix-command 'tviti/org-map)
(global-set-key (kbd "C-c o") #'tviti/org-map)
(define-key 'tviti/org-map (kbd "a") #'org-agenda)
(define-key 'tviti/org-map (kbd "c") #'counsel-org-capture)
(define-key 'tviti/org-map (kbd "S") #'org-save-all-org-buffers)
(define-key 'tviti/org-map (kbd "r") #'org-refile)
(define-key 'tviti/org-map (kbd "l") #'org-store-link)
(define-key 'tviti/org-map (kbd "C-a d") #'org-agenda-day-view)
(define-key 'tviti/org-map (kbd "C-a w") #'org-agenda-week-view)
(define-key 'tviti/org-map (kbd "C-a m") #'org-agenda-month-view)
(define-key 'tviti/org-map (kbd "C-a y") #'org-agenda-year-view)
(define-key 'tviti/org-map (kbd "f") #'org-cycle-agenda-files)
(define-key 'tviti/org-map (kbd "d") #'org-decrypt-entry)
(define-key 'tviti/org-map (kbd "b r") #'org-refile-goto-last-stored)
(define-key 'tviti/org-map (kbd "b c") #'org-capture-goto-last-stored)

(define-key 'tviti/org-map (kbd "o") #'org-clock-out)
(define-key 'tviti/org-map (kbd "i") #'org-clock-in-last)
(define-key 'tviti/org-map (kbd "g") #'org-clock-goto)
(define-key 'tviti/org-map (kbd "z") #'org-resolve-clocks)
(define-key 'tviti/org-map (kbd "j") #'counsel-org-goto-all)

(with-eval-after-load 'org-ql
  (define-key 'tviti/org-map (kbd "s") #'org-ql-search)
  (define-key 'tviti/org-map (kbd "v") #'org-ql-view))

;; counsel keybindings
(define-prefix-command #'tviti/counsel-map)
(global-set-key (kbd "C-c c") #'tviti/counsel-map)
(define-key 'tviti/counsel-map (kbd "f") #'counsel-git)
(define-key 'tviti/counsel-map (kbd "g") #'counsel-git-grep)
(define-key 'tviti/counsel-map (kbd "G") #'counsel-grep)
(define-key 'tviti/counsel-map (kbd "o") #'counsel-outline)
(define-key 'tviti/counsel-map (kbd "l") #'counsel-locate)
(define-key 'tviti/counsel-map (kbd "v") #'ivy-push-view)
(define-key 'tviti/counsel-map (kbd "V") #'ivy-pop-view)
(define-key 'tviti/counsel-map (kbd "k") #'counsel-kmacro)
(define-key 'tviti/counsel-map (kbd "r") #'counsel-register)
(define-key 'tviti/counsel-map (kbd "p") #'counsel-list-processes)

;; counsel keybindings that live outside of the prefix-map
(global-set-key (kbd "C-c C-r") #'ivy-resume)
(global-set-key (kbd "C-x B") #'counsel-switch-buffer)
(global-set-key (kbd "C-x C-M-b") #'counsel-ibuffer)
(with-eval-after-load 'evil
  (define-key 'tviti/counsel-map (kbd "c e") #'counsel-evil-marks)
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
(global-set-key (kbd "C-c s") #'shell)

(global-set-key (kbd "C-c d Y") #'tviti/copy-buffer-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hawaiian language layer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't forget that you can also very effectively insert a kahakō by using the
;; TeX input method (i.e. "C-x <ret> C-\ TeX"), although this doesn't give the
;; ʻokina.
(define-prefix-command 'tviti/hawaiian-map)
(global-set-key (kbd "C-c h") #'tviti/hawaiian-map)

(define-key 'tviti/hawaiian-map (kbd "'") (lambda () (interactive) (insert ?ʻ)))

;; Lowercase kahakou chars
(define-key 'tviti/hawaiian-map (kbd "a") (lambda () (interactive) (insert ?ā)))
(define-key 'tviti/hawaiian-map (kbd "e") (lambda () (interactive) (insert ?ē)))
(define-key 'tviti/hawaiian-map (kbd "i") (lambda () (interactive) (insert ?ī)))
(define-key 'tviti/hawaiian-map (kbd "o") (lambda () (interactive) (insert ?ō)))
(define-key 'tviti/hawaiian-map (kbd "u") (lambda () (interactive) (insert ?ū)))

;; Uppercase kahakou chars
(define-key 'tviti/hawaiian-map (kbd "A") (lambda () (interactive) (insert ?Ā)))
(define-key 'tviti/hawaiian-map (kbd "E") (lambda () (interactive) (insert ?Ē)))
(define-key 'tviti/hawaiian-map (kbd "I") (lambda () (interactive) (insert ?Ī)))
(define-key 'tviti/hawaiian-map (kbd "O") (lambda () (interactive) (insert ?Ō)))
(define-key 'tviti/hawaiian-map (kbd "U") (lambda () (interactive) (insert ?Ū)))

;; "Free up" conflicts between Shell-mode and eyebrowse
;; TODO: These are local, not GLOBAL keybindings!
(with-eval-after-load 'eyebrowse
  (define-key shell-mode-map eyebrowse-keymap-prefix nil))

(global-set-key (kbd "C-c w") #'tviti/evil-window)

(provide 'global-keys)
