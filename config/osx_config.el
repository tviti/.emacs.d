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
