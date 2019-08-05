;; Custom keybindings
(add-hook 'latex-mode-hook
	  (lambda () (local-set-key (kbd "\C-c ;") 'comment-region)))
(add-hook 'latex-mode-hook
	  (lambda () (local-set-key (kbd "\C-u \C-c ;") 'uncomment-region)))
