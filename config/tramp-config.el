(require 'tramp)

;; force tramp to honor remote hosts' .bash_profile
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

(provide 'tramp-config)
