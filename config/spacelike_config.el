;; Configs inspired by playing around in spacemacs for a little bit

;; Helm
;; (require 'helm)
;; (require 'helm-config)
;; (setq helm-mode nil)

;; which-key
(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; highlight-numbers and rainbow-delimiters
(require 'highlight-numbers)
(require 'rainbow-delimiters)
	 (add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
