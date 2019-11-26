;; Trendy mode: Make the first window borderless
;; (setq initial-frame-alist '((undecorated . t)))

;; Package manager configuration
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Enable/disable specific packages
(setq package-load-list '(all (jupyter nil)))

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.  You may delete these
;; explanatory comments.
(package-initialize)

;;
;; Load custom configuration files
;;
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

;; Configs we want loaded immediately
(if (string= system-type "darwin")
    (require 'macos-config))

(require 'user-globals)
(require 'user-functions)

(require 'completion-config)
(require 'evil-config)
(require 'spacelike-config)
(require 'global-keys)
(require 'feeds-config)
(require 'tramp-config)
(require 'slime-config)
(require 'ruler-mode-config)
(require 'python-config)
(require 'org-config)
(require 'matlab-config)
(require 'lsp-config)
(require 'latex-mode-config)
(require 'julia-config)
(require 'ess-config)

;; This should be one of the last things loaded, since it uses mode-hooks for
;; toggling linters.
(require 'linter-config)

;;
;; Misc configurations
;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(winner-mode 1) ;; Enables window state undo/redo

;; Enable the eyebrowse-mode "window manager"
(eyebrowse-mode t)

;; Make dired report human-readable file sizes
(setq dired-listing-switches "-alh")

;; Pick a font + size
(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono")

;; Enable line number and relative line numbering using the new built in system
;; (requires Emacs >= 26.1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)
			    (setq display-line-numbers 'relative)))

;; Store bookmarks in a different directory
(setq bookmark-file (expand-file-name "bookmarks" tviti/sync-dir))

;; Store yasnippets in a different directory
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs
	       (expand-file-name "yasnippets" tviti/sync-dir)))

;;
;; Setup groupings for ibuffer
;;
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("nix" (filename . ".*?\\."))
	 ("PDFs" (filename . ".*?\\.pdf"))
	 ("dired" (mode . dired-mode))
	 ("org-mode" (mode . org-mode))
	 ("magit" (mode . magit-status-mode)))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Tell Ediff to NOT create a whole frame for the control window (this will fuck
;; us completely if we are using a tiling window manager like yabai).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use next-browser for browse-url functionality.
(setq browse-url-browser-function #'tviti/browse-url-next-browser)

(desktop-save-mode 1)
(setq ring-bell-function 'ignore)

;; Don't pollute this file with vars set using the customization interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;
;; Load a theme.
;;
(require 'ruler-mode-config)
(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'spacemacs-dark t)
	    (tviti/ruler-match-theme)))
