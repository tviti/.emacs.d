;; key rebindings
(setq mac-option-modifier   'meta
      mac-command-modifier  'super
      mac-function-modifier 'hyper)

;; Fix for "unknown/untrusted cert signing authority" error from
;; package manager, per
;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; Make the font a little bit bigger for my laptop
(set-face-attribute 'default nil :height 180)
(set-face-attribute 'default nil :family "Inconsolata")

;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(warn "You have a bandaid fix for
https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 implemented. This
bug should be fixed come Emacs 26.3. Once the newer version is
released, you should install it, and disable this fix in your
.init.el")
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
