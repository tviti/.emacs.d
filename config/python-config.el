(elpy-enable)

;; Force elpy to use python3 for backend language features
(setq elpy-rpc-python-command "python3")

;; Use jupyter console, or on os x use ipython with workaround for
;; https://github.com/jorgenschaefer/elpy/issues/1550
;; (if (string-equal system-type "darwin")
;;     (setq python-shell-interpreter "ipython"
;; 	  python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")
  ;; (progn
  ;;   (setq python-shell-interpreter "jupyter"
  ;; 	  python-shell-interpreter-args "console --simple-prompt"
  ;; 	  python-shell-prompt-detect-failure-warning nil)
  ;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;; 		 "jupyter")))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")


;; in "Virtual Envs" menu, show me conda envs
;; (if (string-equal system-name "R-Daneel.local")
;;     (setenv "WORKON_HOME" "~/anaconda2/envs"))

;; Launch a Jupyter Notebook server (uses the active pyvenv)
(defun jupyter-nb-server-launch ()
  (interactive)
  (let ((buffer "*Jupyter Notebook Server*"))
    (async-shell-command  "jupyter-notebook" buffer buffer)))
			
(provide 'python-config)
