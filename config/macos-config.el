;; key rebindings
(setq mac-option-modifier   'super
      mac-command-modifier  'meta
      mac-function-modifier 'hyper)

;; Fix for "unknown/untrusted cert signing authority" error from
;; package manager, per
;; https://blog.vifortech.com/posts/emacs-tls-fix/
;; (require 'gnutls)
;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; Necessary to make yabai play nicely with Emacs
(menu-bar-mode t)

;; Instruct Emacs to consume Apple-key events (e.g. cmd-Q and cmd-H)
(setq mac-pass-command-to-system nil)

;; Use mdfind for the locate
(setq locate-command "mdfind")
(with-eval-after-load 'counsel
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

;; Doesn't work on windows, so we only enable it on macOS.
(require 'vterm)
(add-hook 'vterm-mode-hook (lambda ()
			  (setq-local global-hl-line-mode nil)  ;; Causes flickering
			  (evil-collection-init '(vterm))))
(add-hook 'vterm-copy-mode-hook (lambda()
			     (call-interactively 'hl-line-mode)))


;; Set the theme based on the system theme and update it automatically.
(require 'solarized-theme)

(defun tviti/update-theme-from-system (appearance)
  "Update the Emacs theme based on the macOS system appearance."
  (let* ((new-theme (if (eq appearance 'dark)
                        'solarized-selenized-dark
                      'solarized-selenized-light)))
    (unless (eq tviti/theme new-theme)
      (when tviti/theme
        (disable-theme tviti/theme))
      (setq tviti/theme new-theme)
      (load-theme new-theme t))))

(add-to-list 'ns-system-appearance-change-functions 'tviti/update-theme-from-system)

;; Load initial theme based on current system appearance
(add-hook 'after-init-hook (lambda () (tviti/update-theme-from-system ns-system-appearance)))
  
(provide 'macos-config)
