;; Prettification of `ruler-mode' ruler. So far, this is only tested using the
;; Spacemacs theme, and so may look like shit with other themes.

;; FIXME: This breaks for some reason if there is a `matlab-mode' buffer open
;; during startup.

;; Set the fill-column width for use in ruler-mode.
;; 80 column IBM punch card. How retro...
(require 'ruler-mode)
(add-hook 'ruler-mode-hook (lambda () (setq fill-column 80)))

;; Turn on ruler-mode in all code buffers
(add-hook 'prog-mode-hook (lambda () (ruler-mode 1)))
;; Turn on ruler-mode in buffers associated w/ files
(add-hook 'find-file-hook (lambda () (ruler-mode 1)))

(defun tviti/ruler-match-theme ()
  "Set ruler faces to better match the currently active theme."
  (set-face-attribute 'ruler-mode-default nil :background
		      (face-attribute 'default :background))
  (set-face-attribute 'ruler-mode-default nil :foreground
		      (face-attribute 'line-number :foreground))
  (set-face-attribute 'ruler-mode-default nil :box
		      (face-attribute 'vertical-border :foreground))

  (set-face-attribute 'ruler-mode-column-number nil :foreground
		      (face-attribute 'line-number :foreground))
  (set-face-attribute 'ruler-mode-fill-column nil :foreground
		      (face-attribute 'mode-line-buffer-id :foreground))
  (set-face-attribute 'ruler-mode-comment-column nil :foreground
		      (face-attribute 'font-lock-comment-face :foreground))
  (set-face-attribute 'ruler-mode-current-column nil :foreground
		      (face-attribute 'font-lock-warning-face :foreground)))

;; Make sure the ruler is re-styled when the theme is changed.
(add-hook 'after-init-hook (lambda () (setq ruler-mode-initial-display nil)))
(add-hook 'enable-theme-functions (lambda (_theme) (tviti/ruler-match-theme)))

(provide 'ruler-mode-config)
