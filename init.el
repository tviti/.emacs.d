;; Package manager configuration
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Enable/disable specific packages
(setq package-load-list '(all
                          (jupyter nil)))

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.  You may delete these
;; explanatory comments.
(package-initialize)

;; Load a theme. For some reason, on a "partial-theme-load" if we don't want
;; until the entire init.el is finished running, hence the after-init-hook
;; FIXME: Best way to do this is still to use custom-theme picker gui. We still
;; fire off load-theme over here though so that the theme faces are also
;; available throughout this script.

;; (load-theme 'spacemacs-dark t)

;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (load-theme 'spacemacs-dark t)))
;; (load-theme 'solarized-dark t)

;; Hide the tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable the eyebrowse-mode "window manager"
(eyebrowse-mode t)

;; Make dired report human-readable file sizes
(setq dired-listing-switches "-alh")

;; Use flyspell mode in text-mode buffers (e.g. org-mode), but NOT in
;; change-log-mode or log-edit-mode. Taken from
;; https://www.emacswiki.org/emacs/FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Spell check comments in .el files using flyspell
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

;; Pick a font + size
(cond ((string= (system-name) "R-Daneel.local")
       ;; Make the font a little bit bigger for my laptop
       (progn
	 (set-face-attribute 'default nil :height 180)
	 (set-face-attribute 'default nil :family "Inconsolata")))
      (t
       (set-face-attribute 'default nil :height 140)
       (set-face-attribute 'default nil :family "Inconsolata")))

;; Enable line number and relative line numbering using the new built in system
;; (requires Emacs >= 26.1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)
			    (setq display-line-numbers 'relative)))

;; Store bookmarks in a different directory
(setq bookmark-file "~/Sync/bookmarks")

;; Setup groupings for ibuffer
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("org-mode" (mode . org-mode))
	 ("magit" (mode . magit-status-mode)))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Tell Ediff to NOT create a whole frame for the control window (this will fuck
;; us completely if we are using a tiling window manager like yabai).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Load custom configuration files
(if (string-equal system-type "darwin")
    (load-file ".emacs.d/config/osx_config.el"))
(mapc 'load-file '(".emacs.d/config/completion_config.el"
		   ".emacs.d/config/ESS_config.el"
		   ".emacs.d/config/global_keys.el"
		   ".emacs.d/config/evil_config.el"
		   ".emacs.d/config/magit_config.el"
		   ".emacs.d/config/tramp_config.el"
		   ".emacs.d/config/python_config.el"
		   ".emacs.d/config/latex-mode_config.el"
		   ".emacs.d/config/matlab_config.el"
		   ".emacs.d/config/julia_config.el"
		   ".emacs.d/config/org_config.el"
		   ".emacs.d/config/spacelike_config.el"
		   ".emacs.d/config/lsp_config.el"
		   ".emacs.d/config/slime_config.el"
		   ".emacs.d/config/ruler-mode_config.el"))

;; User defined functions
(defun kill-all-buffers ()
  "Kill all buffers, save for a few \"special\" ones."
  (interactive)
  (let ((save-list '("*scratch*" "*Warnings*" "*Messages*"))
	(blist (buffer-list)))
    (if (y-or-n-p "Are you sure you want to kill all buffers?")
	(mapc (lambda (b)
		(unless (member (buffer-name b) save-list)
		  (kill-buffer b)))
	      blist))))

(defun copy-buffer-string ()
  "Copy the entire current buffer to the \"kill-ring\" (i.e. clipboard)."
  (interactive)
  (kill-new (buffer-string)))

(defun copy-buffer-name ()
  "Copy the name of the active buffer to the kill-ring. Useful for swapping
  buffers between eyebrowse workspaces."
  (interactive)
  (kill-new (buffer-name)))

(desktop-save-mode 1)
(setq ring-bell-function 'ignore)

;; Don't pollute this file with vars set using the customization interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
