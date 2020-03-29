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
  ;; Set the fill-column in emacs to be consistent with the default text limit
  ;; indicator location in the matlab gui editor
  (set-fill-column 75)
  
  ;; Line numbers don't work by default for some reason. Turn them on.
  (display-line-numbers-mode)
  (setq display-line-numers 'relative))

(defun tviti/M-shell-mode-setup ()
  "Apply configuration for `M-shell-mode'.")

;; Don't use company for completions, since the company style menu is a little
;; bit of a context switch from the ivy menu.
(custom-set-variables
 '(matlab-shell-tab-use-company nil))

(define-key matlab-shell-mode-map (kbd "C-M-i") #'matlab-shell-tab)

(add-hook 'matlab-mode-hook #'tviti/matlab-mode-setup)
(add-hook 'M-shell-mode #'tviti/M-shell-mode-setup)

(provide 'matlab-config)
