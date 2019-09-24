;; Global keybindings
(global-set-key (kbd "\C-c ;") 'comment-region)
(local-set-key (kbd "\C-u \C-c ;") 'uncomment-region)

;; Suggested flyspell keybindings, per
;; https://www.emacswiki.org/emacs/FlySpell. Note that this is not ALL of the
;; suggested keybindings, just the ones that I "liked".
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
