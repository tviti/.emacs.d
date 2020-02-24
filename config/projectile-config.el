(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
(setq projectile-completion-system 'ivy)

;; Make projectile-switch-project popup a magit-status buffer
(setq projectile-switch-project-action #'projectile-dired)

(provide 'projectile-config)
