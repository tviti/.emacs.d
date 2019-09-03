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
(if (string-equal system-type "darwin")
    (setq powerline-height 20))
(spaceline-spacemacs-theme)