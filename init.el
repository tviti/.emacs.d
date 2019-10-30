;; Trendy mode: Make the first window borderless
;; (setq initial-frame-alist '((undecorated . t)))

;; Hide the tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

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

;;
;; Load a theme.
;;
;; TODO: The following note may no longer be an issue (setting the theme to nil
;; in custom.el seems like it may have fixed this).
;; For some reason, on a "partial-theme-load" if we don't want until the entire
;; init.el is finished running, hence the after-init-hook FIXME: Best way to do
;; this is still to use custom-theme picker gui. We still fire off load-theme
;; over here though so that the theme faces are also available throughout this
;; script.
;; (load-theme 'solarized-light t)
(load-theme 'spacemacs-light t)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'solarized-dark t)

(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'spacemacs-light t)))

;; User specific globals
;; TODO: Some of my config files depend on these values being defined! It may be
;; better to lump these into a separate, and then use the (require ...)
;; mechanism in my config files to ensure they exist!
(defvar tviti/sync-dir "~/Sync"
  "A synchronized directory.")

(defvar tviti/emacs-dir "~/.emacs.d"
  "The directory to use as/instead of .emacs.d")


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

;; Use next-browser for `browse-url' functionality
(defvar tviti/next-browser-command ""
  "Path to next-browser, used by `tviti/browse-url-next-browser'")

(defun tviti/browse-url-next-browser (url &rest args)
  (start-process "next-browser"
		 nil tviti/next-browser-command url))

(setq tviti/next-browser-command
      (cond
       ((string= (system-name) "R-Daneel.local")
	"/Applications/Next.app/Contents/MacOS/next")
       (t
	"next")))

(setq browse-url-browser-function #'tviti/browse-url-next-browser)

;; Load custom configuration files
(if (string-equal system-type "darwin")
    (load-file (concat tviti/emacs-dir "/config/osx_config.el")))

(defvar tviti/config-files '("completion_config.el"
			     "ESS_config.el"
			     "global_keys.el"
			     "evil_config.el"
			     "magit_config.el"
			     "tramp_config.el"
			     "python_config.el"
			     "latex-mode_config.el"
			     "matlab_config.el"
			     "julia_config.el"
			     "org_config.el"
			     "spacelike_config.el"
			     "lsp_config.el"
			     "slime_config.el"
			     "feeds_config.el"
			     "ruler-mode_config.el")
  "List of user config files to be loaded from located in
  <TVITI/EMACS-DIR>/config")

(dolist (fn tviti/config-files)
  (load-file (format "%s/config/%s" tviti/emacs-dir fn)))

;;
;; User defined functions
;;

(defun tviti/toggle-frame-undecorated ()
  "Toggle decorations (i.e. borders) in the active frame."
  (interactive)
  (let ((undecorated (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not undecorated))))

(defun tviti/kill-all-buffers ()
  "Kill all buffers, save for a few \"special\" ones."
  (interactive)
  (let ((save-list '("*scratch*" "*Warnings*" "*Messages*"))
	(blist (buffer-list)))
    (when (y-or-n-p "Are you sure you want to kill all buffers?")
      ;; TODO: Use dolist instead of mapc
      (mapc (lambda (b)
	      (unless (member (buffer-name b) save-list)
		(kill-buffer b)))
	    blist))))

(defun tviti/copy-buffer-string ()
  "Copy the entire current buffer to the \"kill-ring\" (i.e. clipboard)."
  (interactive)
  (kill-new (buffer-string)))

(defun tviti/copy-buffer-name ()
  "Copy the name of the active buffer to the kill-ring. Useful for swapping
  buffers between eyebrowse workspaces."
  (interactive)
  (kill-new (buffer-name)))

(desktop-save-mode 1)
(setq ring-bell-function 'ignore)

;; Don't pollute this file with vars set using the customization interface
(setq custom-file (concat tviti/emacs-dir "/custom.el"))
(load custom-file)
