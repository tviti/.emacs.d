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
(evil-collection-init 'ediff)
(evil-collection-init 'ibuffer)
(evil-collection-init 'package-menu)
(evil-collection-init 'dired)
(evil-collection-init 'magit)
(evil-collection-init 'bookmark)

;; Make org-mode more evil. Config is per the README.md
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
