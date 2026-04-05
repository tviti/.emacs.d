;; Trendy mode: Make the first window borderless
;; (setq initial-frame-alist '((undecorated . t)))

;; Package manager configuration. NOTE: This is actually pointless to do on a
;; machine running nixpkgs! I'm leaving it here anyways though for whenever I
;; inevitably try to bootstrap this emacs config on a box w/o nix.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.  You may delete these
;; explanatory comments.
(package-initialize)

;; Pick a font + size
(add-to-list 'default-frame-alist
	     '(font . "Iosevka-14"))

;; Drop straight into server mode, so that the the email daemon(s) can update
;; the index can be updated using emacsclient.
(server-start)

;; Load packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Load this early so that the shell env is active during init
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH" "PYTHONPATH")))

(use-package spacemacs-theme)

(use-package magit)
(use-package magit-annex)

(use-package markdown-mode)

(use-package csv-mode)

(use-package direnv)

(use-package nix-mode)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :config (setq copilot-idle-delay nil)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)))

(use-package pcre2el)

(use-package slime)

;;
;; Load custom configuration files
;;
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/nix-flymake" user-emacs-directory))

;; MCP config for copilot-chat.el
;; (setq epg-pinentry-mode 'loopback)
;; (load-library "copilot-chat-config.el.gpg")

;; Configs we want loaded immediately
(menu-bar-mode 0)
(if (string= system-type "darwin")
    (require 'macos-config))

(require 'user-globals)
(require 'user-functions)
(require 'latex-mode-config)
(require 'completion-config)
(require 'eshell-config)
(require 'evil-config)
(require 'global-keys)
(require 'literate-config)
;; linter-config must precede lsp-config (tviti/setup-latex-lsp references tviti/linter)
(require 'linter-config)
(require 'lsp-config)
(require 'matlab-config)
(require 'org-config)
(require 'project-config)
(require 'python-config)
(require 'ruler-mode-config)
(require 'slime-config)
(require 'spacelike-config)
;;(require 'feeds-config)
(require 'tramp-config)
(require 'mouse-config)
;;
;; Misc configurations
;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(winner-mode 1) ;; Enables window state undo/redo

;; Make dired report human-readable file sizes
(setq dired-listing-switches "-alhFSL")

;; Enable line number and relative line numbering using the new built in system
;; (requires Emacs >= 26.1)
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type 'relative)
				      (display-line-numbers-mode)))

;; Store bookmarks in a different directory
(setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))

;; ;; Store yasnippets in a different directory
;; (with-eval-after-load 'yasnippet
;;   (add-to-list 'yas-snippet-dirs
;; 	       (expand-file-name "yasnippets" tviti/sync-dir)))

;;
;; Setup groupings for ibuffer
;;
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("gpg" (filename . "\\.gpg"))
	 ("nix" (filename . "\\.nix"))
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
;; (setq browse-url-browser-function #'tviti/browse-url-nyxt-browser)
(setq browse-url-browser-function #'browse-url-default-browser)

;; emacs-27 specific customisations
(when (>= emacs-major-version 27)
  (setq tab-bar-show nil))

;; ASK before exiting!
(setq confirm-kill-emacs #'yes-or-no-p)

(desktop-save-mode 1)
(setq ring-bell-function 'ignore)

;; Don't pollute this file with vars set using the customization interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(which-function-mode 1) ;; Show the current function in the mode line

;; Helps to make it a little more obvious which window is active
(setq-default cursor-in-non-selected-windows nil)

;;
;; Load a theme.
;;
(add-hook 'after-init-hook (lambda () (load-theme tviti/theme t)))

;;
;; Tramp setup
;;
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))
