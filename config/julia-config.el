(require 'julia-mode)

;; Fix for https://github.com/JuliaEditorSupport/julia-emacs/issues/5
;; I.e. the mysterious "why the fuck is everything suddenly flush left"
(setq julia-max-block-lookback 20000)

(add-hook 'julia-mode-hook
	  (lambda ()
	    (setq show-trailing-whitespace t)))

;; Highlight certain key words. Taken from
;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'julia-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
		1
		font-lock-warning-face t)))))

;; Julia-repl specific setup
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; I don't think this helps much, but load the term-mode evil-collection so that
;; we can get "better" evil integration with julia-repl (which uses term-mode)
(require 'evil-collection)
(evil-collection-init 'term)

(defun julia-repl-mini ()
  "Split the current window horizontally into 2/3 & 1/3 portions, then create a
julia-repl in the bottom portion"
  (interactive)
  (setq size (truncate ; Determine number of lines for new buffer
	      (* -1.0
		 (/ (window-total-height) 3.0))))
  (split-window-below size)
  (windmove-down) 
  (switch-to-buffer ; Create the julia-repl, and then place in current window
   (julia-repl--start-and-setup (julia-repl--get-executable-key)
				julia-repl-inferior-buffer-name-suffix)))
(add-hook 'julia-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c z") 'julia-repl-mini)))

;; ;; Trying out Jupyter for our repl
;; (require 'jupyter)

;; ;; Make emacs aware of the Jupyter kernel installed by Conda.jl
;; (add-to-list 'exec-path "~/.julia/conda/3/bin")


;; ;; Display output from a connected buffer in the repl
;; (setq jupyter-repl-echo-eval-p t)

(provide 'julia-config)
