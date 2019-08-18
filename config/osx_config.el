;; key rebindings
(setq mac-option-modifier   'meta
      mac-command-modifier  'super
      mac-function-modifier 'hyper)

;; Fix for "unknown/untrusted cert signing authority" error from
;; package manager, per
;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
