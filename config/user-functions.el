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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; netCDF interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'transient)

(defun tviti/ncdump (&rest args)
  "Call ncdump with ARGS."
  (let ((bname "*nc-dump Output*"))
    (when (get-buffer bname)
      (with-current-buffer bname (erase-buffer)))
    (apply #'call-process "ncdump" nil bname nil args)
    (display-buffer bname)))

(defun tviti/dired-do-ncdump (&optional args)
  "Call ncdump on file at point, prompting the user for args."
  (interactive (list (tviti/dired-ncdump-arguments)))
  (let* ((fn (dired-get-filename))
	 (args (if args
		   args
		 (split-string (read-shell-command "ncdump args: "))))
	 (args-out (append args (list fn))))
    (apply #'tviti/ncdump args-out)))

(define-transient-command tviti/dired-ncdump ()
  "Call ncdump on file at point."
  :man-page "ncdump"
  ["Arguments"
   ("-h" "Show only the header information in the output" ("-h" "-h"))
   ("-c" "Show the values of coordinate variables" ("-c" "-c"))
   (tviti/dired-ncdump:-v)]
  [("RET" "go!" tviti/dired-do-ncdump)])

(defun tviti/dired-ncdump-arguments nil
  (transient-args 'tviti/dired-ncdump))

(defun tviti/ncdump-get-vars (fn)
  "Returns a list of variables contained in FN."
  (with-temp-buffer
    (apply #'call-process "ncdump" nil (buffer-name) nil (list "-h" fn))
    ;; Delete everything EXCEPT for the variable list
    (goto-char (point-min))
    (re-search-forward "^variables:$")
    (delete-region (point-min) (point))
    (re-search-forward "^// global attributes:$")
    (re-search-backward "^// global attributes:$")
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (let ((vars))
      (while (re-search-forward "[a-z]+ \\(.+?\\)(.*?).*" nil t)
	(pushnew (match-string 1) vars))
      vars)))

(define-infix-argument tviti/dired-ncdump:-v ()
  :description "Limit to variable(s)"
  :class 'transient-option
  :key "-v"
  :argument "-v"
  :reader (lambda (prompt initial-input history)
	    (completing-read
	     prompt
	     (tviti/ncdump-get-vars (dired-get-filename))
	     nil t initial-input history)))

;; TODO: This is not the right place for keybindings!
(define-key dired-mode-map (kbd "C-c d n") #'tviti/dired-ncdump)

(provide 'user-functions)
