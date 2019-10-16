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

;; Keybindings
(require 'org)
(define-key org-mode-map (kbd "C-c C-j") nil)
(define-key org-mode-map (kbd "C-c C-j") #'counsel-org-goto)

;;;;;;;;;;;;;;
;; ido-mode ;;
;;;;;;;;;;;;;;

;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)
