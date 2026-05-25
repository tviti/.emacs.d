;; Package Manifest
;; This file centralizes all package declarations to minimize entropy.
;; Configuration for these packages is managed in their respective files in config/

(require 'use-package)
(setq use-package-always-ensure t)

(when (string= system-type "darwin")
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH" "PYTHONPATH" "NIX_PATH"))))

;; Themes
(use-package spacemacs-theme :ensure t)
(use-package solarized-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package zenburn-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package kaolin-themes :ensure t)
(use-package ef-themes :ensure t)

(use-package magit :ensure t)
(use-package magit-annex :ensure t)

(use-package csv-mode :ensure t)

(use-package direnv :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(unless (eq system-type 'windows)
  (use-package vterm :ensure t))
(use-package pcre2el :ensure t)

(use-package gptel :ensure t
  :vc (:rev :newest)
  :config
  (add-hook 'gptel-context-buffer-mode-hook
	    (lambda ()
	      (define-key gptel-context-buffer-mode-map (kbd "C-c C-n") #'gptel-context-next)
	      (define-key gptel-context-buffer-mode-map (kbd "C-c C-p") #'gptel-context-previous))))
(use-package gptel-agent :ensure t)

;; Language modes
(use-package matlab-mode :ensure t)
(use-package tex :ensure auctex)
(use-package reftex :ensure t)
(use-package polymode :ensure t)
(use-package julia-mode :ensure t)
(use-package julia-repl :ensure t)
(use-package slime :ensure t)
(use-package nix-mode :ensure t)
(use-package markdown-mode :ensure t)

;; UI enhancements
(use-package which-key :ensure t)
(use-package highlight-numbers :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package spaceline :ensure t)
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; Evil/Vim
(use-package evil :ensure t)
(use-package evil-collection :ensure t)
(use-package evil-org :ensure t)
(use-package flyspell-correct-ivy :ensure t)

;; Completion
(use-package ivy :ensure t)
(use-package counsel :ensure t)
(use-package bash-completion :ensure t)

;; Org-mode ecosystem
(use-package org-ql :ensure t)
(use-package org-bullets :ensure t)
(use-package ox-gfm :ensure t)
(use-package htmlize :ensure t)
(use-package doct :ensure t)

;; PDF tools
(use-package pdf-tools :ensure t)

;; Snippets
(use-package yasnippet :ensure t)

(provide 'packages)
