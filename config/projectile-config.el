(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
(setq projectile-completion-system 'ivy)

(provide 'projectile-config)
