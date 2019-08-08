(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Starts the Emacs server
;; (necessary for SyncTex integration w/ Skim)
(server-start)

;; Auto-raise Emacs on activation
(defun raise-emacs-on-aqua() 
    (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

(tool-bar-mode -1)
(setq ring-bell-function 1)
(scroll-bar-mode -1)

(defalias 'bclose 'kill-buffer)
(defalias 'bc 'bclose)

;; Load custom configuration files
(if (string-equal system-type "darwin")
    (load-file ".emacs.d/config/osx_config.el"))
(load-file ".emacs.d/config/ESS_config.el")
(load-file ".emacs.d/config/global_keys.el")
(load-file ".emacs.d/config/evil_config.el")
(load-file ".emacs.d/config/magit_config.el")
(load-file ".emacs.d/config/tramp_config.el")
(load-file ".emacs.d/config/python_config.el")
(load-file ".emacs.d/config/linum-relative_config.el")
;;(load-file "./config/matlab_config.el")
;;(load-file "./config/org_config.el")
;; (load-file "./config/popwin_config.el")
;; (load-file "./config/idlwave_config.el")
;; (load-file "./config/company-mode_config.el")
;; (load-file "./config/org-mode_config.el")
;; (load-file "./config/latex-mode_config.el")
;; (load-file "./config/polymode-config.el")
;; (load-file "./config/ein-config.el")
;; (load-file "./config/ess-config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode nim-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode apples-mode)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "89dd0329d536d389753111378f2425bd4e4652f892ae8a170841c3396f5ba2dd" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ess-S-assign "_")
 '(ess-indent-with-fancy-comments nil)
 '(explicit-shell-file-name nil)
 '(fci-rule-color "#383838")
 '(font-use-system-font t)
 '(linum-relative-mode t t)
 '(matlab-fill-code t)
 '(matlab-show-mlint-warnings t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-M-RET-may-split-line (quote ((default))))
 '(org-babel-load-languages
   (quote
    ((latex . t)
     (python . t)
     (emacs-lisp . t)
     (matlab . t))))
 '(org-icalendar-categories (quote (local-tags category todo-state)))
 '(org-icalendar-include-body t)
 '(org-icalendar-include-todo (quote all))
 '(org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo todo-due)))
 '(org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
 '(package-selected-packages
   (quote
    (evil-magit elpy org pdf-tools evil-collection ein stan-mode markdown-mode polymode ess arduino-mode zenburn-theme request-deferred python-environment popwin nzenburn-theme magit linum-relative goto-last-change evil epc company-quickhelp alert ac-ispell)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Inconsolata")))))

(desktop-save-mode 1)
(setq ring-bell-function 'ignore)
