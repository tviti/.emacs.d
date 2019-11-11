;;;;;;;;;;;;;;;;;;
;; Auctex setup ;;
;;;;;;;;;;;;;;;;;;

(require 'latex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Enable reftex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; Enable line numbers
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (display-line-numbers-mode)
	    (setq display-line-numbers 'relative)))

;; Note: the combo luatex + synctex + Evince is problematic (the resulting docs
;; wind up crashing Evince).
(setq-default TeX-engine 'xetex)

;; Enable synctex doc correlation
(setq TeX-source-correlate-mode t)

;; Auctext uses just a single key to toggle comment-uncomment, which throws me
;; off (since I'm used to pre-fixed bindings for uncomment).
(define-key LaTeX-mode-map (kbd "\C-c ;") 'comment-region)
(define-key LaTeX-mode-map (kbd "\C-u \C-c ;") 'uncomment-region)

;;;;;;;;;;;;;;;;;;;;;
;; pdf-tools setup ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'pdf-tools)
;; The semantics are somewhat peculiar, but this function serves as the
;; entry-point for both pdf-tools installation, AND setup.
(pdf-tools-install)

;; Set emacs as the default pdf viewer (for this session), so that texdoc
;; doesn't try to use an external viewer for command documentation.
(setenv "PDFVIEWER" "emacsclient -n")

;; Don't blink the cursor if evil-mode is activated. Adapted from
;; https://github.com/hlissner/doom-emacs/pull/1107
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (when evil-mode
	      (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))

;; Select programs for viewing compilation results
(setq TeX-view-program-selection
      '((output-dvi "open")
	(output-pdf "PDF Tools")
	(output-html "open")))

;; Revert pdf-view buffer after compilation
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Highlight certain key words. Taken from
;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defun tviti/latex-font-lock ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

(add-hook 'LaTeX-mode-hook #'tviti/latex-font-lock)

;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet setup ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode-on)

(provide 'latex-mode-config)
