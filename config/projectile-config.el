;; Built-in project.el configuration

;; Set dired as default action when switching projects
(setq project-switch-use-all-projects t)

;; Define a keymap for project commands bound to C-c p
(defvar project-command-map (make-sparse-keymap)
  "Keymap for project.el commands")

(define-key project-command-map (kbd "f") #'project-find-file)
(define-key project-command-map (kbd "d") #'project-find-dir)
(define-key project-command-map (kbd "p") #'project-switch-project)
(define-key project-command-map (kbd "s") #'project-shell)
(define-key project-command-map (kbd "c") #'project-compile)

(global-set-key (kbd "C-c p") project-command-map)

(provide 'projectile-config)
