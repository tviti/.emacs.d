;; User defined globals
(setq evil-want-keybinding nil)

(defvar tviti/sync-dir "~/Sync"
  "A synchronized directory.")

(defvar tviti/nyxt-browser-command "nyxt"
  "Command called by `tviti/browse-url-nyxt-browser'.")

(defvar tviti/evil-leader (kbd "SPC")
  "Key to use as `evil-mode''s leader-key.")

(defvar tviti/theme 'nord)
(defvar tviti/light-theme 'nord)
(defvar tviti/dark-theme 'nord)

(provide 'user-globals)
