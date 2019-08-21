(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; Fix for https://github.com/JuliaEditorSupport/julia-emacs/issues/5
;; I.e. the mysterious "why the fuck is everything suddenly flush left"
(setq julia-max-block-lookback 20000)

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