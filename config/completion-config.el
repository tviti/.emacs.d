;; Setup blocks experimenting with different completion engines.

;;;;;;;;;;;;;;
;; ivy-mode ;;
;;;;;;;;;;;;;;
(use-package ivy)
(use-package counsel)

;; Configuration for the ivy/counsel/swiper trifecta
(ivy-mode 1)
(counsel-mode 1)
;; Customizations suggested by the *info* page
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

;; SLIME won't actually use the ivy backend ootb. Per abo-abo: "Looks like SLIME
;; is really old school, and doesn't obey the completion-in-region-function
;; API. We can make it work with advice":
(use-package slime)
(defun ora-slime-completion-in-region (_fn completions start end)
  (funcall completion-in-region-function start end completions))

(advice-add
 'slime-display-or-scroll-completions
 :around #'ora-slime-completion-in-region)

;; Keybindings setup. Only keybindings that replace external functionality
;; should be placed here. The proper place for opponent's keybindings is
;; global_keys.el
(require 'org)
(define-key org-mode-map [remap org-goto] #'counsel-org-goto)
(define-key org-mode-map [remap org-set-tags-command] #'counsel-org-tag)

(global-set-key (kbd "C-s") #'swiper-isearch)

;; swiper-isearch won't work in pdf's (and may even cause Emacs to
;; hang). Override its keybinding in PDF-view buffers.
(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-s") #'isearch-forward))))

;;
;; ivy + emacs 27's tab-bar mode
;;
;; (defun tviti/ivy-switch-tab-action (tab-name)
;;   "Switch to TAB-NAME or, if TAB-NAME does not name a tab, then create a new tab
;; with that name."
;;   (if (member tab-name (mapcar (lambda (x) (alist-get 'name x))
;; 			       (tab-bar-tabs)))
;;       (tab-bar-switch-to-tab tab-name)
;;     (tab-bar-new-tab)
;;     (tab-bar-rename-tab tab-name)))

(defun tviti/ivy-switch-tab-action (tab-name)
  "Switch to TAB-NAME or, if TAB-NAME does not name a tab, then create a new tab
with that name."
  (unless (zerop (length tab-name))
    (if (member tab-name (mapcar (lambda (x) (alist-get 'name x))
				 (tab-bar-tabs)))
	(tab-bar-switch-to-tab tab-name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name))))

(defun tviti/recent-tabs (string &optional predicate flag)
  (ivy--re-filter string (mapcar (lambda (tab)
				(alist-get 'name tab))
			      (tab-bar--tabs-recent))))

(defun tviti/ivy-switch-tab ()
  "Switch to another tab."
  (interactive)
  (ivy-read "Tab name: " #'tviti/recent-tabs
	    :action #'tviti/ivy-switch-tab-action
	    :dynamic-collection t
	    :caller 'tviti/ivy-switch-tab))

(defun tviti/ivy--rename-tab-action (tab-name)
  (interactive)
  (let ((new-name (read-from-minibuffer
                   "New name for tab (leave blank for automatic naming): "
		   nil nil nil nil tab-name)))
    (if (zerop (length tab-name))
	(tab-bar-rename-tab-by-name (alist-get 'name (tab-bar--current-tab)) new-name)
      (tab-bar-rename-tab-by-name tab-name new-name))
    (ivy--reset-state ivy-last)))

(defun tviti/ivy--close-tab-action (tab-name)
  (unless (zerop (length tab-name))
    (tab-bar-close-tab-by-name tab-name)
    (ivy--reset-state ivy-last)))

(ivy-set-actions
 'tviti/ivy-switch-tab
 '(("k" tviti/ivy--close-tab-action "close tab")
   ("r" tviti/ivy--rename-tab-action "rename tab")))

;;
;; Custom ivy-switch-buffer actions
;;
(defun tviti/ivy--switch-buffer-other-frame-action (buffer)
  "Switch to BUFFER in other frame.
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer-other-frame ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (find-file-other-frame (cdr virtual))
        (switch-to-buffer-other-frame buffer)))))

(defun tviti/ivy--switch-buffer-other-tab-action (buffer)
  "Switch to BUFFER in other tab.
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer-other-tab ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (find-file-other-tab (cdr virtual))
        (switch-to-buffer-other-tab buffer)))))

(ivy-add-actions
 'ivy-switch-buffer
 '(("J" tviti/ivy--switch-buffer-other-frame-action "other frame")
   ("t" tviti/ivy--switch-buffer-other-tab-action "other tab")))

(defun tviti/ivy--switch-project-other-frame-action (project)
  (interactive)
  (let ((projectile-switch-project-action 'projectile-dired-other-frame))
    (projectile-switch-project-by-name project)))

(defun tviti/ivy--switch-project-other-window-action (project)
  (interactive)
  (let ((projectile-switch-project-action 'projectile-dired-other-window))
    (projectile-switch-project-by-name project)))

(with-eval-after-load 'projectile
  (ivy-add-actions
   'projectile-switch-project
   '(("j" tviti/ivy--switch-project-other-window-action "other window")
     ("J" tviti/ivy--switch-project-other-frame-action "other frame"))))
 
;; (defun tviti/counsel-switch-tab-update-fn ()
;;   (let ((names (mapcar (lambda (x) (alist-get 'name x))
;; 		       (tab-bar-tabs)))
;; 	(selection (ivy-state-current ivy-last)))
;;     (when (member selection names)
;;       (save-excursion ;; Prevents point jumping to (point-min) on tab switch
;; 	(tab-bar-switch-to-tab selection)))))

;; (defun tviti/counsel-switch-tab ()
;;   ""
;;   (interactive)
;;   (let* ((ivy-update-fns-alist
;; 	  '((tviti/ivy-switch-tab . tviti/counsel-switch-tab-update-fn)))
;; 	 (starting-tab (tab-bar--current-tab))
;; 	 (res))
;;     (unwind-protect
;; 	(setq res (tviti/ivy-switch-tab))
;;       (tab-bar-switch-to-tab (if res
;; 				 res
;; 			       (alist-get 'name starting-tab))))))

;;;;;;;;;;;;;;
;; ido-mode ;;
;;;;;;;;;;;;;;

;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)

(provide 'completion-config)
