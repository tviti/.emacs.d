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
(evil-collection-init '(ediff ibuffer package-menu dired magit bookmark slime))

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

(provide 'evil-config)
