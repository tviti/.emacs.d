;;
;; Elfeed setup
;;
(require 'elfeed)
(require 'ivy)

(setq elfeed-db-directory "~/Sync/elfeed/")

;; Load the list of feeds
(let ((fn "~/Sync/elfeed-feeds.el"))
  (with-temp-buffer
    (insert-file-contents-literally fn)
    (setq elfeed-feeds (read (buffer-string)))))

(defun tviti/elfeed-search-tag-all ()
  "Same as `elfeed-search-tag-all' (called interactively), but
using `completing-read' for tag selection."
  (interactive)
  (let ((tag-str (completing-read "Tag: " (elfeed-db-get-all-tags) nil nil)))
    (elfeed-search-tag-all (intern tag-str))))

(defun tviti/elfeed-show-tag ()
  (interactive)
  (let ((tag-str (completing-read "Tag: " (elfeed-db-get-all-tags) nil nil)))
    (elfeed-show-tag (intern tag-str))))

(defun tviti/elfeed-search-untag-all ()
  "Same as `elfeed-search-untag-all' (called interactively), but
using `completing-read' for tag selection."
  (interactive)
  (let* ((entries (elfeed-search-selected))
	 (tag-list))
    (mapc (lambda (entry-tags)
	    (dolist (tag (elfeed-entry-tags entry-tags))
	      (unless (member tag tag-list) (push tag tag-list))))
	  entries)
    (let ((tag-str (completing-read "Tag: " tag-list nil t)))
      (elfeed-search-untag-all (intern tag-str)))))

(defun tviti/elfeed-ivy-filter-complete (str)
  "Completion function for `tviti/elfeed-ivy-live-filter'. STR should be a
(potentially) valid `elfeed-search-filter'. If the last directive
of STR corresponds to a positive/negative tag match directive,
then return a list of filtered tag completions."
  (let* ((directive (car (last (split-string str " "))))
	 (prefix (unless (string-empty-p directive) (substring directive 0 1))))
    ;; If the last directive was a tag directive, bring up tag completions
    (when (and (or (string= prefix "+")
		   (string= prefix "-"))
	       (string-match "\\(.*[+-]\\)" str))
      (let ((substr (match-string 1 str))
	    (tags (mapcar #'symbol-name (elfeed-db-get-all-tags))))
	;; It would be nice to use ONLY the tags for the completion list, but
	;; it's simpler to use the entire input line w/ the tags appended.
	(mapcar (lambda (tag)
		  (format "%s%s" substr tag))
		(ivy--re-filter (substring directive 1) tags))))))

(defun tviti/elfeed-ivy-live-filter ()
  "An implementation of `elfeed-live-filter' that uses `ivy' to backend for
building incremental tag completions."
  (interactive)
  (unwind-protect
      (let ((elfeed-search-filter-active :live))
	(setq elfeed-search-filter
	      (ivy-read "Filter: " #'tviti/elfeed-ivy-filter-complete
			:initial-input elfeed-search-filter
			:dynamic-collection t
			:require-match nil))
	(elfeed-search-update :force))))

;; Keybindings for elfeed+evil
(defun tviti/elfeed-search-evil-setup ()
  "Evil keybindings for `elfeed-search-mode'"
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    (kbd "RET") #'elfeed-search-show-entry
    "+" #'tviti/elfeed-search-tag-all
    "-" #'tviti/elfeed-search-untag-all
    "u" #'elfeed-search-tag-all-unread
    "r" #'elfeed-search-untag-all-unread
    "s" #'tviti/elfeed-ivy-live-filter
    "S" #'elfeed-search-set-filter
    "c" #'elfeed-search-clear-filter)

  (evil-collection-define-key 'visual 'elfeed-search-mode-map
    "+" #'tviti/elfeed-search-tag-all
    "-" #'tviti/elfeed-search-untag-all
    "r" #'elfeed-search-untag-all-unread))

(defun tviti/elfeed-show-evil-setup ()
  "Keybindings for `elfeed-show-mode'"
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "+" #'tviti/elfeed-show-tag
    "-" #'elfeed-show-untag))

(eval-after-load 'evil-collection
  (add-hook 'elfeed-search-mode-hook #'tviti/elfeed-search-evil-setup))

(eval-after-load 'evil-collection
  (add-hook 'elfeed-show-mode-hook #'tviti/elfeed-show-evil-setup))
