(require 'matlab)
(matlab-cedet-setup)

;; Uncomment these two lines to run a remote MATLAB session from MBP
;; (setq matlab-shell-command "/Applications/MATLAB_R2015a.app/bin/matlab")
;; (setq matlab-mode-install-path "/Users/taylor/.emacs.d/matlab-emacs")

;; shell mode keybindings
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c o") 'matlab-shell-close-current-figure)))
;; (add-hook 'matlab-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; editor mode keybindings
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c a") 'matlab-shell-close-figures)))
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c \C-c") 'matlab-shell-run-cell)))
;; (add-hook 'matlab-mode-hook
;; 	  (lambda () (local-set-key (kbd "\C-c v") 'matlab-shell-clear-all)))

;; change comment string to be compatible with matlab IDE
(setq matlab-comment-region-s "% ")

(defun tviti/matlab-mode-setup ()
  "Apply configuration for `matlab-mode'."
  ;; Set the fill-column in emacs to be consistent with the default text limit
  ;; indicator location in the matlab gui editor
  (set-fill-column 75)
  
  ;; Line numbers don't work by default for some reason. Turn them on.
  (display-line-numbers-mode)
  (setq display-line-numbers 'relative)
  (add-to-list 'mlint-programs "maci64/mlint"))

(defun tviti/M-shell-mode-setup ()
  "Apply configuration for `M-shell-mode'."
  (define-key matlab-shell-mode-map (kbd "C-c C-n") #'comint-next-input))

;; Don't use company for completions, since the company style menu is a little
;; bit of a context switch from the ivy menu.
(custom-set-variables
 '(matlab-shell-tab-use-company nil))

(define-key matlab-shell-mode-map (kbd "C-M-i") #'matlab-shell-tab)

(add-hook 'matlab-mode-hook #'tviti/matlab-mode-setup)
(add-hook 'M-shell-mode #'tviti/M-shell-mode-setup)

(with-eval-after-load 'ob-octave
  ;; (defun org-babel-edit-prep:matlab (info)
  ;;   ;; Enable mlint and the debugger, by assigning a temporary file to the
  ;;   ;; `org-edit-special' buffer. MATLAB and mlint are fussy about hyphenated
  ;;   ;; filenames, so we avoid using `org-babel-temp-file' for this.
  ;;   (set-visited-file-name (make-temp-file "octave_" nil ".m" (buffer-string)))
  ;;   ;; ;; Save to trigger mlint.
  ;;   (save-buffer))

  ;; Hacked to suppress input lines in result blocks when using matlab-mode.
  (defun org-babel-octave-evaluate-session
      (session body result-type &optional matlabp)
    "Evaluate BODY in SESSION."
    (let* ((tmp-file (org-babel-temp-file (if matlabp "matlab-" "octave-")))
	   (wait-file (org-babel-temp-file "matlab-emacs-link-wait-signal-"))
	   (full-body
	    (pcase result-type
	      (`output
	       (if 'matlab-shell
		   (format "cd %s;\n%s" ;; Make sure we eval in the correct dir
			   (file-name-directory (buffer-file-name)) body)
		 (mapconcat
		  #'org-babel-chomp
		  (list body org-babel-octave-eoe-indicator) "\n")))
	      (`value
	       (mapconcat
		#'org-babel-chomp
		(list (format org-babel-octave-wrapper-method
			      body
			      (org-babel-process-file-name tmp-file 'noquote)
			      (org-babel-process-file-name tmp-file 'noquote))
		      org-babel-octave-eoe-indicator) "\n"))))
	   (raw (if (and 'matlab-shell (not (eq result-type 'value)))
		    (save-window-excursion
		      (with-temp-buffer
			(insert full-body)
			(write-region nil nil tmp-file nil t)
			(split-string
			 (matlab-shell-collect-command-output
			  (matlab-shell-region-command (point-min) (point-max)))
			 "\\(K\\|EDU\\)?>> *")))
		  (org-babel-comint-with-output
		      (session
		       (if matlabp
			   org-babel-octave-eoe-indicator
			 org-babel-octave-eoe-output)
		       t full-body)
		    (insert full-body) (comint-send-input nil t)))) results)
      (pcase result-type
	(`value
	 (org-babel-octave-import-elisp-from-file tmp-file))
	(`output
	 (setq results
	       (if matlabp
		   (if (and 'matlab-shell (not (eq result-type 'value)))
		       (reverse (remove "" (mapcar #'org-strip-quotes
						 (mapcar #'org-trim raw))))
		     (cdr (reverse (delq "" (mapcar #'org-strip-quotes
						    (mapcar #'org-trim raw))))))
		 (cdr (member org-babel-octave-eoe-output
			      (reverse (mapcar #'org-strip-quotes
					       (mapcar #'org-trim raw)))))))
	 (mapconcat #'identity (reverse results) "\n"))))))


(provide 'matlab-config)
