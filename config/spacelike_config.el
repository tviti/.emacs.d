;; Configs inspired by playing around in spacemacs for a little bit

;; (require 'helm)
;; (require 'helm-config)
;; (setq helm-mode nil)

(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

(require 'highlight-numbers)
(require 'rainbow-delimiters)
	 (add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'spaceline-config)
(setq powerline-default-separator 'slant)
;; On my laptop, the mode-line height needs to be finagled a little bit,
;; but this looks like crap on my office computer (which runs Centos-7)
(if (string-equal (system-name) "R-Daneel.local")
    (setq powerline-height 20))
(spaceline-spacemacs-theme)

;; Change the color of the mode line based on the input mode (insert, normal, or
;; Emacs). Spacemacs doesn't do this, but I'm placing it in this file anyways
;; because it's a spaceline config option.
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

;; Show the currently active org-clock in the modeline. This requires turning
;; something off (in this case minor modes) to make space for it
(spaceline-toggle-org-clock)
(spaceline-toggle-minor-modes-off)
