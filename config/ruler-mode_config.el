;; Settings for custom mode-line
(require 'spaceline-config)
(setq powerline-default-separator 'slant)
;; On my laptop, the mode-line height needs to be finagled a little bit,
;; but this looks like crap on my office computer (which runs Centos-7)
(if (string-equal system-type "darwin")
    (setq powerline-height 20))
(spaceline-spacemacs-theme)

;; Set the fill-column width for use in ruler-mode.
;; 80 column IBM punch card. How retro...
(require 'ruler-mode)
(add-hook 'ruler-mode-hook (lambda () (setq fill-column 80)))

;; Turn on ruler-mode in buffers associated w/ files
(add-hook 'find-file-hook (lambda () (ruler-mode 1)))

;; Try not to make ruler-mode stick out like such a sore thumb
;; FIXME: This is fragile, and only works if all the used face-attributes are
;; already set
(add-hook 'after-init-hook
	  (lambda ()
	    (set-face-attribute 'ruler-mode-default nil :background
				(face-attribute 'default :background))
	    (set-face-attribute 'ruler-mode-default nil :foreground
				(face-attribute 'linum :foreground))
	    (set-face-attribute 'ruler-mode-default nil :box
				(face-attribute 'vertical-border :foreground))

	    (set-face-attribute 'ruler-mode-column-number nil :foreground
				(face-attribute 'linum :foreground))
	    (set-face-attribute 'ruler-mode-fill-column nil :foreground
				(face-attribute 'mode-line-buffer-id :foreground))
	    (set-face-attribute 'ruler-mode-comment-column nil :foreground
				(face-attribute 'font-lock-comment-face :foreground))
	    (set-face-attribute 'ruler-mode-current-column nil :foreground
				(face-attribute 'font-lock-warning-face :foreground))))

