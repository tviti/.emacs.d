;; Configs inspired by playing around in spacemacs for a little bit
;;
(which-key-mode)
(which-key-setup-minibuffer)

;; Enable subtle visual helpers in programming buffers
(add-hook 'prog-mode-hook #'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;
;; Powerline configuration
;;
(require 'spaceline-config)
(setq powerline-default-separator 'utf-8)
;; (setq powerline-height 20) ;; Give it some more breathing room
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


(defun tviti/spaceline-normalize-modeline-faces (&optional frame)
  "Make spaceline modelines render consistently on terminal FRAMEs.
  With some fonts, spaceline tends to mess up the vertical alignment of
  the over/underline. It's easier to just remove it instead of trying to
  fix it, and the effect isn't very noticable anyways."
  (when (and frame (not (display-graphic-p frame)))
    (dolist (face '(mode-line mode-line-active mode-line-inactive
                     mode-line-highlight
                     powerline-active0 powerline-active1 powerline-active2
                     powerline-inactive0 powerline-inactive1 powerline-inactive2))
      (set-face-attribute face frame :box nil :overline nil :underline nil))))

(defun tviti/spaceline-compile ()
  "After-theme-load cleanup for spaceline."
  (spaceline-compile)
  (dolist (frame (frame-list))
    (tviti/spaceline-normalize-modeline-faces frame)))

;; Make sure the modeline is recompiled when a theme is loaded
(add-hook 'after-make-frame-functions #'tviti/spaceline-normalize-modeline-faces)
(add-hook 'enable-theme-functions (lambda (_theme) (tviti/spaceline-compile)))

(provide 'spacelike-config)
