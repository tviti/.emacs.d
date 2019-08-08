(require 'undo-tree)
(global-undo-tree-mode)

(require 'evil)
(evil-mode 1)

;; Move all elements of evil-emacs-state-modes to evil-motion-state-modes
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; Enable evil where it normally wouldn't be enabled
(evil-collection-init 'ediff)
(evil-collection-init 'ibuffer)
(evil-collection-init 'package-menu)
(evil-collection-init 'dired)
