(require 'matlab)
(matlab-cedet-setup)

;; Uncomment these two lines to run a remote MATLAB session from MBP
;; (setq matlab-shell-command "/Applications/MATLAB_R2015a.app/bin/matlab")
;; (setq matlab-mode-install-path "/Users/taylor/.emacs.d/matlab-emacs")

;; On my office computer, we have to use software opengl for rendering
(if (string-equal (system-name) "magneto")
    (setq matlab-shell-command-switches '("-nodesktop" "-softwareopengl")))

;; shell mode keybindings
(add-hook 'matlab-shell-mode-hook
	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
(add-hook 'matlab-shell-mode-hook
	  (lambda () (local-set-key (kbd "\C-c o") 'matlab-shell-close-current-figure)))
(add-hook 'matlab-shell-mode-hook
	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; editor mode keybindings
(add-hook 'matlab-mode-hook
	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
(add-hook 'matlab-mode-hook
	  (lambda () (local-set-key (kbd "\C-c \C-c") 'matlab-shell-run-cell)))
(add-hook 'matlab-mode-hook
	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; change comment string to be compatible with matlab IDE
(setq matlab-comment-region-s "% ")
