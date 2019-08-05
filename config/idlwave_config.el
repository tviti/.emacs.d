(setq idlwave-shell-explicit-file-name "gdl")

;; restore evil mode window switching behaviour
(add-hook 'idlwave-shell-mode-hook
	  (lambda() (local-unset-key (kbd "\C-w"))))
