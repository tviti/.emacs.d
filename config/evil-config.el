;; Fix related to https://github.com/emacs-evil/evil-collection/issues/60
;; evil-want-keybinding must be set before evil loads (done in init.el)

(evil-mode 1)

(require 'user-globals)


;; Enable evil where it normally wouldn't be enabled
(add-to-list 'evil-collection-key-blacklist (key-description tviti/evil-leader))
(evil-collection-init '(ediff ibuffer package-menu dired magit bookmark slime mu4e))


;; Not sure why, but evil-collection-init won't actually load
;; evil-collection-pdf, so we do it like so instead.
(with-eval-after-load 'pdf-tools (evil-collection-pdf-setup))
 
;; Make org-mode more evil. Config is per the README.md
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))

;; Make Info mode more evil
(defun tviti/evil-Info-mode-setup ()
  "Evil keybindings for `Info-mode'."
  (evil-define-key '(normal visual) Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char
    "u" #'Info-history-back
    "g g " #'evil-goto-first-line)

  (evil-set-initial-state 'Info-mode 'normal))

(with-eval-after-load 'evil
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
(define-key evil-normal-state-map [remap ispell-word] #'flyspell-correct-wrapper)

;;
;; Leader-key bindings
;;
(evil-set-leader '(normal visual motion) tviti/evil-leader)

;; SPC as leader will be shadowed by certain mode-maps. Clear these bindings.
(with-eval-after-load 'magit-mode
   (define-key magit-mode-map tviti/evil-leader nil))

(with-eval-after-load 'dired
   (define-key dired-mode-map tviti/evil-leader nil))

(with-eval-after-load 'magit-stash
  (define-key magit-stash-mode-map tviti/evil-leader nil))

(with-eval-after-load 'magit-diff
  (define-key magit-diff-mode-map tviti/evil-leader nil))

(with-eval-after-load 'magit-diff
  (define-key magit-revision-mode-map tviti/evil-leader nil))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map tviti/evil-leader nil)
  (evil-define-key 'motion org-agenda-mode-map
    tviti/evil-leader nil))

(evil-define-key '(normal visual motion) 'global
  (kbd "<leader>h") #'evil-window-left
  (kbd "<leader>j") #'evil-window-down
  (kbd "<leader>k") #'evil-window-up
  (kbd "<leader>l") #'evil-window-right

  (kbd "<leader>f") #'find-file
  (kbd "<leader>s") #'save-buffer
  (kbd "<leader>b") #'switch-to-buffer

  (kbd "<leader>0") #'delete-window

  (kbd "<leader>i") #'imenu
  (kbd "<leader>e") #'eshell

  (kbd "<leader>x") #'Control-X-prefix)

(with-eval-after-load 'counsel
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>B") #'counsel-switch-buffer
    (kbd "<leader>rb") #'counsel-bookmark
    (kbd "<leader>c") #'tviti/counsel-map))

(when (>= emacs-major-version 27)
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>t") #'tviti/ivy-switch-tab))

(with-eval-after-load 'global-keys
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>m") #'tviti/magit-map
    (kbd "<leader>o") #'tviti/org-map
    (kbd "<leader>O") #'org-capture))

(with-eval-after-load 'project-config
  (defun tviti/project-switch-project-dired ()
    "Switch to a project and open its root directory in dired."
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (dired project-dir)))
  
  (define-key evil-normal-state-map (kbd "<leader>p") project-prefix-map)
  (define-key evil-visual-state-map (kbd "<leader>p") project-prefix-map)
  (define-key evil-motion-state-map (kbd "<leader>p") project-prefix-map)
  (define-key project-prefix-map "p" #'tviti/project-switch-project-dired))

(with-eval-after-load 'gptel
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>a") #'tviti/gptel-map))


(provide 'evil-config)
