;; User defined globals
(setq evil-want-keybinding nil)

(defvar tviti/sync-dir "~/Sync"
  "A synchronized directory.")

(defvar tviti/nyxt-browser-command "nyxt"
  "Command called by `tviti/browse-url-nyxt-browser'.")

(defvar tviti/evil-leader (kbd "SPC")
  "Key to use as `evil-mode''s leader-key.")

(defvar tviti/theme 'solarized-selenized-dark)

(provide 'user-globals)
