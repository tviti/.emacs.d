;; Setup blocks experimenting with different completion engines.

;;;;;;;;;;;;;;
;; ivy-mode ;;
;;;;;;;;;;;;;;

;; Configuration for the ivy/counsel/swiper trifecta
(ivy-mode 1)
(counsel-mode 1)

;; Customizations suggested by the *info* page
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; SLIME won't actually use the ivy backend ootb. Per abo-abo: "Looks like SLIME
;; is really old school, and doesn't obey the completion-in-region-function
;; API. We can make it work with advice":
(require 'slime)
(defun ora-slime-completion-in-region (_fn completions start end)
  (funcall completion-in-region-function start end completions))

(advice-add
 'slime-display-or-scroll-completions
 :around #'ora-slime-completion-in-region)

;; Keybindings
(require 'org)
(define-key org-mode-map [remap org-goto] #'counsel-org-goto)
(define-key org-mode-map [remap org-set-tags-command] #'counsel-org-tag)

;;;;;;;;;;;;;;
;; ido-mode ;;
;;;;;;;;;;;;;;

;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)
