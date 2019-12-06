;;
;; User defined functions
;;
(require 'user-globals)

(defun tviti/toggle-frame-undecorated ()
  "Toggle decorations (i.e. borders) in the active frame."
  (interactive)
  (let ((undecorated (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not undecorated))))

(defun tviti/kill-all-buffers ()
  "Kill all buffers, save for a few \"special\" ones."
  (interactive)
  (let ((save-list '("*scratch*" "*Warnings*" "*Messages*"))
	(blist (buffer-list)))
    (when (y-or-n-p "Are you sure you want to kill all buffers? ")
      ;; TODO: Use dolist instead of mapc
      (mapc (lambda (b)
	      (unless (member (buffer-name b) save-list)
		(kill-buffer b)))
	    blist))))

(defun tviti/copy-buffer-string ()
  "Copy the entire current buffer to the \"kill-ring\" (i.e. clipboard)."
  (interactive)
  (kill-new (buffer-string)))

(defun tviti/copy-buffer-name ()
  "Copy the name of the active buffer to the `kill-ring'.
Useful for swapping buffers between eyebrowse workspaces."
  (interactive)
  (kill-new (buffer-name)))

(defun tviti/copy-buffer-directory ()
  "Copy the directory of the current buffer to the `kill-ring'."
  (interactive)
  (when default-directory
    (kill-new default-directory)))

(defun tviti/browse-url-next-browser (url &rest args)
  "Start a new Next browser process to view URL `tviti/next-browser-command'.
If Next is already running, this will instead open a new Next
buffer at URL, in the last active browser window.  ARGS is
included for call signature compatibility, but is otherwise ignored."
  (start-process "next-browser"
		 nil tviti/next-browser-command url))

(defun tviti/mac-port-p ()
  "Check if the running Emacs instance is Mitsuhara Yamamoto's mac-port."
  (when (and (eq window-system 'mac) (boundp 'mac-carbon-version-string))
    t))

(defun tviti/macos-dark-toggle ()
  "Toggle between light/dark mode on macOS."
  (interactive)
  (if (tviti/mac-port-p)
      (mac-osa-script
       "tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode")))

(provide 'user-functions)
