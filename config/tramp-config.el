(require 'tramp)

;; force tramp to honor remote hosts' .bash_profile
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

;; Interactive connection to UHHPC's xCAT cluster. This should be called as a
;; multi-hop connection (e.g. "C-x C-f <user>@<login-node>|xcati:self@<partition>")
(add-to-list 'tramp-methods
	     '("xcati"
	       (tramp-login-program "srun")
	       (tramp-login-args
		(("-I")
		 ("-p" "%h") ; Partition to request
		 ("-c" "1") ; Num. cpus to request
		 ("--mem=6G")
		 ("-t" "60") ; Timeout
		 ("--pty" "/bin/sh")))
	       (tramp-login-env (("SHELL") ("/bin/sh")))
	       (tramp-remote-shell "/bin/sh")
	       (tramp-remote-shell-login
		("-l"))
	       (tramp-remote-shell-args ("-c"))))

;; rclone Google Drive "Shared With Me" folder
(add-to-list 'tramp-methods
	     '("rclone-shared"
	       (tramp-mount-args nil)
	       (tramp-copyto-args nil)
	       (tramp-moveto-args nil)
	       (tramp-about-args ("--full" "--drive-shared-with-me"))))


(provide 'tramp-config)
