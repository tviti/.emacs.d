;; Fix related to https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-keybinding nil)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'evil)
(evil-mode 1)

;; Move all elements of evil-emacs-state-modes to evil-motion-state-modes
;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;; (setq evil-emacs-state-modes nil)

;; Enable evil where it normally wouldn't be enabled
(require 'evil-collection)
(evil-collection-init '(ediff ibuffer package-menu dired magit bookmark slime mu4e))

(with-eval-after-load 'magit
  (require 'evil-magit))

;; Not sure why, but evil-collection-init won't actually load
;; evil-collection-pdf, so we do it like so instead.
(with-eval-after-load 'pdf-tools (evil-collection-pdf-setup))
 
;; Make org-mode more evil. Config is per the README.md
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; Make Info mode more evil
(defun tviti/evil-Info-mode-setup ()
  "Evil keybindings for `Info-mode'."
  (evil-define-key '(normal visual) Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char
    "u" #'Info-history-back
    "g g " #'evil-goto-first-line)

  (evil-set-initial-state 'Info-mode 'normal))

(eval-after-load 'evil-mode
  (tviti/evil-Info-mode-setup))

(with-eval-after-load 'ess-r-mode
  (evil-set-initial-state 'ess-help-mode 'normal))

;; Make flymake more evil
(with-eval-after-load 'flymake
  (evil-collection-flymake-setup))

;; Make xref more evil
(with-eval-after-load 'xref
  (evil-collection-xref-setup))

;; Override "z=" binding for ispell-word to use the flyspell-correct pkg
(require 'flyspell-correct-ivy)
(define-key evil-normal-state-map [remap ispell-word] #'flyspell-correct-wrapper)

;;
;; Leader-key bindings
;;
(evil-set-leader '(normal visual motion) (kbd "SPC"))

;; SPC as leader will be shadowed by certain mode-maps. Clear these bindings.
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "SPC") nil))

;; dired-mode-map is already being modified by evil-mode, so we use
;; evil-define-key instead.
(evil-define-key 'normal dired-mode-map (kbd "SPC") nil)

(with-eval-after-load 'pdf-view
  (evil-define-key 'normal pdf-view-mode-map (kbd "SPC") nil))
  

(evil-define-key '(normal visual motion) 'global
  (kbd "<leader>h") #'evil-window-left
  (kbd "<leader>j") #'evil-window-down
  (kbd "<leader>k") #'evil-window-up
  (kbd "<leader>l") #'evil-window-right

  (kbd "<leader>f") #'find-file
  (kbd "<leader>s") #'save-buffer
  (kbd "<leader>b") #'switch-to-buffer

  (kbd "<leader>0") #'delete-window)

(with-eval-after-load 'counsel
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>rb") #'counsel-bookmark))

(when (>= emacs-major-version 27)
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>t RET") #'tab-bar-select-tab-by-name
    (kbd "<leader>t 2") #'tab-bar-new-tab
    (kbd "<leader>t r") #'tab-bar-rename-tab))

(with-eval-after-load 'magit-mode
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>ms") #'magit-status
    (kbd "<leader>ml") #'magit-log-buffer-file))

(with-eval-after-load 'projectile
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>p") #'projectile-command-map))

(provide 'evil-config)
