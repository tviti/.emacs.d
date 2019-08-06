(require 'undo-tree)
(global-undo-tree-mode)

(require 'evil)
(evil-mode 1)

;; move all elements of evil-emacs-state-modes to evil-motion-state-modes
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
