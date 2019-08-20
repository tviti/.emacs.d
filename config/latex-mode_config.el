(defun raise-emacs-on-aqua ()
  "Auto-raise emacs on activation"
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))

(defun skim-config ()
  "Setup integration w/ Skim pdf viewer, per \
https://sourceforge.net/p/skim-app/wiki/TeX_and_PDF_Synchronization/#setting-up-emacs. Do \
a shift-cmd-click from within Skim to jump to the associated line in emacs"
  (server-start)  ; Should we check if a daemon is already running?
  (add-hook 'server-switch-hook 'raise-emacs-on-aqua))

;; Might be worth-while expanding this to run on *nix' too. Does Skim run on
;; Centos? Do we also want this to only run when tex-mode activated?
(if (string-equal system-type "darwin")
    (skim-config))

;; Use lualatex for compilation, since this has much bettter unicode support
;; than pdflatex
(add-hook 'tex-mode-hook
	  (lambda ()
	    (add-to-list 'tex-compile-commands
			 '("lualatex %r.tex" nil "%r.pdf") t)))

;; Highlight certain key words in code (this would actually be super
;; useful for other modes as well...)
;; Taken from http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'tex-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
