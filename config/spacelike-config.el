;; Configs inspired by playing around in spacemacs for a little bit
;;
(use-package which-key)
(which-key-mode)
(which-key-setup-minibuffer)

(use-package spacemacs-theme
             :defer t
             :init (load-theme 'spacemacs-light t))

(use-package highlight-numbers)
(use-package rainbow-delimiters)
	 (add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;
;; Powerline configuration
;;
(use-package spaceline
             :config
             (require 'spaceline-config))
(setq powerline-default-separator 'slant)
(setq powerline-height 20) ;; Give it some more breathing room
(spaceline-spacemacs-theme)

;; Change the color of the mode line based on the input mode (insert, normal, or
;; Emacs). Spacemacs doesn't do this, but I'm placing it in this file anyways
;; because it's a spaceline config option.
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

;;
;; Custom spaceline segments
;;
(when (>= emacs-major-version 27)
  (spaceline-define-segment workspace-number
    "Display the name of the active window configuration from `tab-bar-mode'.

NOTE: This overrides the default workspace-number segment def in
spaceline-segments.el"
    (let* ((current-tab (cdr (tab-bar--current-tab)))
	   (explicit-name (alist-get 'explicit-name current-tab))
	   (tab-name (alist-get 'name current-tab)))
      (when explicit-name
	tab-name))))

(spaceline-toggle-org-clock-on)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-version-control-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)

;; Show the current system time
(setq display-time-default-load-average nil)
(setq display-time-format "%a %m/%d %R")
(display-time-mode 1)

;; Only show org-clock total for today
(with-eval-after-load 'org
  (setq org-clock-mode-line-total 'today))

(spaceline-compile)

(provide 'spacelike-config)
