(require 'vterm)

(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm)
  (add-hook 'julia-repl-hook (lambda () (julia-repl-mode 1)))
  (add-hook 'julia-mode-hook (lambda () (julia-repl-mode 1))))

(with-eval-after-load 'eglot
  (setq eglot-connect-timeout 60)  ; JETLS can take a while
  (add-to-list 'eglot-server-programs
               '(((julia-mode :language-id "julia")
                  (julia-ts-mode :language-id "julia"))
                 "jetls"
                 "serve"
                 "--socket"
                 :autoport)))

(provide 'julia-config)
