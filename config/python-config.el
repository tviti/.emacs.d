;; in "Virtual Envs" menu, show me conda envs
;; (if (string-equal system-name "R-Daneel.local")
;;     (setenv "WORKON_HOME" "~/anaconda2/envs"))

;; Launch a Jupyter Notebook server (uses the active pyvenv)
(defun jupyter-nb-server-launch ()
  (interactive)
  (let ((buffer "*Jupyter Notebook Server*"))
    (async-shell-command  "jupyter-notebook" buffer buffer)))
			
(provide 'python-config)
