;; nix-flymake: Lint nix files using nix-instantiate -*- lexical-binding: t; -*-
(require 'flymake)

(defvar-local nix--flymake-proc nil)

(defvar nix-flymake-command '("nix-instantiate" "--parse" "-"))

(defun nix-flymake (report-fn &rest _args)
  (unless (executable-find (car nix-flymake-command))
    (error "Cannot find a suitable checker"))

  (when (process-live-p nix--flymake-proc)
    (kill-process nix--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq nix--flymake-proc
	    (make-process
	     :name "nix-flymake" :noquery t :connection-type 'pipe
	     :buffer (generate-new-buffer " *nix-flymake*")
	     :command nix-flymake-command
	     :sentinel
	     (lambda (proc _event)
	       (when (eq 'exit (process-status proc))
		 (unwind-protect
		     (when (with-current-buffer source
			     (eq proc nix--flymake-proc))
		       (nix--flymake-parse-output (process-buffer proc) source report-fn))
		   (kill-buffer (process-buffer proc)))))))
      (process-send-region nix--flymake-proc (point-min) (point-max))
      (process-send-eof nix--flymake-proc))))

(defun nix--flymake-parse-output (msg-buffer source report-fn)
  (with-current-buffer msg-buffer
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp
	    (rx line-start
		;; type
		(group-n 1 "error: ")
		;; msg
		(group-n 2 (one-or-more (not (any ":")))) ":"
		;; row
		(group-n 3 (one-or-more num)) ":"
		;; col
		(group-n 4 (one-or-more num)) line-end)
	    nil t)
     for msg = (match-string 2)
     for (beg . end) = (let ((line (string-to-number (match-string 3)))
			     (col (string-to-number (match-string 4))))
			 (flymake-diag-region source line col))
     for type = :warning
     collect (flymake-make-diagnostic source beg end type msg)
     into diags
     finally (funcall report-fn diags))))

(provide 'nix-flymake)
