(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode nim-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode apples-mode))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "f633d825e380caaaefca46483f7243ae9a663f6df66c5fad66d4cab91f731c86" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "89dd0329d536d389753111378f2425bd4e4652f892ae8a170841c3396f5ba2dd" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(ess-S-assign "_")
 '(ess-indent-with-fancy-comments nil)
 '(explicit-shell-file-name nil)
 '(fci-rule-color "#383838")
 '(font-use-system-font t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ignored-local-variable-values
   '((python-shell-interpreter-args . "-i --simple-prompt --InteractiveShell.display_page=True")))
 '(matlab-fill-code t)
 '(matlab-shell-tab-use-company nil)
 '(matlab-show-mlint-warnings t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-M-RET-may-split-line '((default)))
 '(org-babel-load-languages '((latex . t) (python . t) (emacs-lisp . t) (matlab . t)))
 '(org-icalendar-categories '(local-tags category todo-state))
 '(org-icalendar-include-body t)
 '(org-icalendar-include-todo 'all)
 '(org-icalendar-use-deadline '(event-if-not-todo event-if-todo todo-due))
 '(org-icalendar-use-scheduled '(event-if-not-todo event-if-todo todo-start))
 '(package-selected-packages
   '(ox-pandoc elfeed counsel ox-gfm poly-R poly-markdown ess evil-org slime lsp-ui flycheck lsp-mode rainbow-delimiters highlight-numbers which-key helm quelpa matlab-mode auctex eyebrowse solarized-theme rebecca-theme spaceline powerline spacemacs-theme julia-repl bash-completion elpy pdf-tools evil-collection ein stan-mode markdown-mode polymode arduino-mode zenburn-theme request-deferred python-environment popwin nzenburn-theme goto-last-change evil epc company-quickhelp alert ac-ispell))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((Python-shell-interpreter . "ipython")
     (backup-directory-alist
      ("." . ".saves"))
     (org-image-actual-width)
     (python-shell-interpreter-args . "-i --simple-prompt --InteractiveShell.display_page=True")
     (python-shell-interpreter-args . "--simple-prompt -i")
     (python-shell-interpreter . "ipython")
     (org-duration-format . h:mm)
     (org-image-actual-width /
			     (display-pixel-width)
			     6)
     (org-confirm-babel-evaluate)
     (org-crypt-key . "0x28AB181E5EE7C51D")
     (python-shell-interpreter . ipython)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#bc8383")
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
     (360 . "#dc8cc3")))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
