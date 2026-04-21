;; Trendy mode: Make the first window borderless
(setq evil-want-keybinding nil)
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
;; (set-face-attribute 'default nil :font "JetBrains mono-11")
(set-face-attribute 'default nil
  :family "JetBrains mono"
  :height 130  ; 16pt at 10pt = 160 (adjust 150-180 to preference)
  :weight 'regular)
(setq-default line-spacing 0.2) ;; Adjust fraction of line height
;; (setq line-spacing 0.5) ;; Adjust fraction of line height
;; (add-to-list 'default-frame-alist
;; 	     '(font . "JetBrains mono-14"))
;; '(font . "Iosevka-14"))


;; Drop straight into server mode, so that the the email daemon(s) can update
;; the index can be updated using emacsclient.
(require 'server)
(unless (server-running-p)
    (server-start))

;;
;; Orchestrator: load custom configuration files
;;
;; Ensure user-emacs-directory points to this repository when loading init.el directly
(when (and (or (not (boundp 'user-emacs-directory))
               (string= user-emacs-directory "~/.emacs.d/"))
           (or load-file-name buffer-file-name))
  (setq user-emacs-directory
        (expand-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "private/" user-emacs-directory))
;; Ensure nix-flymake submodule is on load-path (needed for config/nix-flymake/nix-flymake.el)
(add-to-list 'load-path (expand-file-name "config/nix-flymake" user-emacs-directory))

;; Load package declarations (centralized use-package manifest)
(require 'packages)

;; Configs we want loaded immediately
(menu-bar-mode 0)
(require 'user-globals)
(cond ((string= system-type "darwin") (require 'macos-config))
      (t (require 'solarized-theme)
	 (setq tviti/theme 'solarized-dark)
	 (add-hook 'after-init-hook (lambda () (load-theme tviti/theme)))))

(require 'user-functions)
(require 'spacelike-config)
(require 'completion-config)
(require 'evil-config)
(require 'global-keys)
(require 'latex-mode-config)
(require 'eshell-config)
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
;;(require 'feeds-config)
(require 'tramp-config)
(require 'mouse-config)
(require 'private-config nil t)
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
(load custom-file 'noerror 'nomessage)

(which-function-mode 1) ;; Show the current function in the mode line

;; Helps to make it a little more obvious which window is active
(setq-default cursor-in-non-selected-windows nil)

;;
;; Tramp setup
;;
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))
