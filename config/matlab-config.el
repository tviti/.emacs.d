(require 'matlab)
(matlab-cedet-setup)

;; Uncomment these two lines to run a remote MATLAB session from MBP
;; (setq matlab-shell-command "/Applications/MATLAB_R2015a.app/bin/matlab")
;; (setq matlab-mode-install-path "/Users/taylor/.emacs.d/matlab-emacs")

;; shell mode keybindings
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c o") 'matlab-shell-close-current-figure)))
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; editor mode keybindings
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c \C-c") 'matlab-shell-run-cell)))
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; change comment string to be compatible with matlab IDE
(setq matlab-comment-region-s "% ")

(defun tviti/matlab-mode-setup ()
  "Apply configuration for `matlab-mode'."
  (company-mode 1)
  ;; Set the fill-column in emacs to be consistent with the default text limit
  ;; indicator location in the matlab gui editor
  (set-fill-column 75)
  
  ;; Line numbers don't work by default for some reason. Turn them on.
  (display-line-numbers-mode)
  (setq display-line-numers 'relative))

(defun tviti/M-shell-mode-setup ()
  "Apply configuration for `M-shell-mode'."
  (company-mode 1))

(add-hook 'matlab-mode-hook #'tviti/matlab-mode-setup)
(add-hook 'M-shell-mode #'tviti/M-shell-mode-setup)

(provide 'matlab-config)
