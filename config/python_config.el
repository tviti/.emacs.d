(elpy-enable)

;; Automatically use the BIG virtualenv (make sure to unset this when we are done with this project!)
;; (shell-command "~/anaconda2/envs/BIG/etc/conda/activate.d/env_vars.sh")
(pyvenv-activate "~/anaconda2/envs/BIG")

;; Use jupyter console, or on os x use ipython with workaround for issue #1550 (https://github.com/jorgenschaefer/elpy/issues/1550)
(if (string-equal system-type "darwin")
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
	       "jupyter"))

;(elpy-use-ipython)
;; (setq elpy-rpc-backend "jedi")  

;; User Defined Keybindings
;; (defun mp-add-python-keys ()
;;   (local-set-key (kbd "C-c ;") 'comment-region))
;; (add-hook 'python-mode-hook 'mp-add-python-keys)
