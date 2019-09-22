;; Enable line number
(add-hook 'LaTeX-mode-hook
	  (lambda () (linum-mode 1)))

;; Use flycheck for tex-file linting
(require 'flycheck)
(add-hook 'LaTeX-mode-hook
	  (lambda () (flycheck-mode 1)))

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
;; than pdflatex. NOTE: This is only works for the emacs built in tex-mode!
;; (add-hook 'tex-mode-hook
;; 	  (lambda ()
;; 	    (add-to-list 'tex-compile-commands
;; 			 '("lualatex %r.tex" nil "%r.pdf") t)))

;; The following are all AUCtex specific
(require 'tex-site)  ; Weird that it's not called 'auctex...

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; Enable synctex correlation
;; (tex-source-correlation-mode t)

;; Highlight certain key words. Taken from
;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; Tell auctex to use lualatex for compilation
;; (setq-default TeX-engine 'luatex)
(setq-default TeX-engine 'xetex) ;; luatex + synctex + evince are problematic


;; Enable synctex doc correlation
(setq TeX-source-correlate-mode t)

;; Default to using Skim displayline for pdf view when we're on OS X Doing C-v
;; from emacs will take you to the corresponding line in Skim Doing cmd-click
;; from Skim will take you to the corresponding line in Emacs
(if (eq system-type 'darwin)
    (setq TeX-view-program-selection
	  '((output-dvi "open")
	    (output-pdf "displayline")
	    (output-html "open"))))

;; Auctext uses just a single key to toggle comment-uncomment, which throws me
;; off (since I'm used to pre-fixed bindings for uncomment).
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (define-key LaTeX-mode-map (kbd "\C-c ;") 'comment-region)
	    (define-key LaTeX-mode-map (kbd "\C-u \C-c ;") 'uncomment-region)))

;; User defined functions
(defun vec-at-point ()
  "Wrap an evil-visual block in a latex \\vec command."
  (interactive)
  (if (evil-visual-state-p)
      (let ((start evil-visual-mark)
	    (end evil-visual-end))
	(kill-region start end)
	(goto-char start)
	(insert (concat "\\vec{" (buffer-substring start end) "\}")))
    (message "An evil-visual block must be selected for this function to work!")))

(defun vec-at-point ()
  "Wrap an evil-visual block in a latex \\vec command."
  (interactive)
  (if (evil-visual-state-p)
      (let ((start evil-visual-mark)
	    (end evil-visual-end)
	    (str (buffer-substring evil-visual-mark evil-visual-end)))
	(kill-region start end)
	(goto-char start)
	(insert (concat "\\vec{" str "\}"))
	(goto-char (+ start 10)))
    (message "An evil-visual block must be selected for this
    function to work!")))
