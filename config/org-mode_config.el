;; org-ical-export-agenda-files throws an error if this is not defined
(setq org-agenda-default-appointment-duration 0)

;; sync org calendars with hawaii.edu server
(defun org-mycal-sync ()
  (interactive)
  (org-icalendar-export-agenda-files)
  (shell-command "rsync -vz --rsync-path=/usr/local/bin/rsync ~/Documents/Spring2017/*.ics tviti@uhunix.hawaii.edu:~/public_html/calendars/"))
