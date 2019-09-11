(require 'linum-relative)
;; (global-linum-mode 1)
;; (linum-relative-global-mode 1)

;; Making linum-relative global is risky (freezes emacs on PDFs)
(add-hook 'prog-mode-hook #'linum-relative-on)
