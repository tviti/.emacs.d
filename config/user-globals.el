;; User defined globals
(defvar tviti/sync-dir "~/Sync"
  "A synchronized directory.")

(defvar tviti/next-browser-command
  (cond ((string= (system-name) "R-Daneel.local")
	 "/Applications/Next.app/Contents/MacOS/next")
	(t
	 "next"))
  "Command called by `tviti/browse-url-next-browser'.")

(provide 'user-globals)
