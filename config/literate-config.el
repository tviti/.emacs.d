;; General facilities for literate programming. The goal here is to implement an
;; interface that exposes common literate programming tasks, while abstracting
;; away the host language.
(require 'transient)
(require 'polymode)
(require 'org)

(defmacro tviti/literate-with-host (&optional poly-body org-body)
  "Execute different body forms, depending on the host literate
programming environment. If `polymode' is active, then expand
POLY-BODY, while if `org-mode' is active, instead activate
ORG-BODY."
  `(cond ((tviti/polymode-active-p)
	  ,poly-body)
	 ((tviti/org-mode-active-p)
	  ,org-body)))

(define-transient-command tviti/literate ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Navigation"
    ("<tab>" "folding"  tviti/literate-fold)
    ("p" "prev chunk" tviti/literate-previous-chunk)
    ("n" "next chunk" tviti/literate-next-chunk)
    ("C-p" "prev heading" outline-previous-visible-heading)
    ("C-n" "next heading" outline-next-visible-heading)
    ("C-e" "scroll down" scroll-up-line)
    ("C-y" "scroll up" scroll-down-line)
    ("o" "outline" counsel-outline)
    ("g" "goto named block" org-babel-goto-named-src-block)]
   ["Editing"
    ("'" "special edit" org-edit-special)
    ("e" "eval chunk" tviti/literate-eval-chunk)
    ("k" "kill chunk" tviti/literate-kill-chunk)
    ("t" "tangle" org-babel-tangle)
    ("m" "mark chunk" tviti/literate-select-chunk)
    (":" "insert header arg" org-babel-insert-header-arg)]
   ["Previewing"
    ("v" "toggle images" tviti/literate-toggle-inline-images)
    ("l" "toggle LaTeX" tviti/literate-toggle-inline-latex)]])
   
(defun tviti/polymode-active-p ()
  "Returns t if `polymode' is active in the current buffer."
  (and (boundp polymode-mode) polymode-mode))

(defun tviti/org-mode-active-p ()
  "Returns t if `org-mode' is active in the current buffer."
  (eq major-mode 'org-mode))

(defun tviti/literate-previous-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-previous-chunk 1)
   (org-previous-block 1)))

(defun tviti/literate-next-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-next-chunk 1))
  (org-next-block 1))

(defun tviti/literate-eval-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-eval-region-or-chunk)
   (org-babel-execute-src-block-maybe)))

(defun tviti/literate-eval-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-eval-region-or-chunk)
   (let ((type (car (org-element-at-point))))
	 (cond
	  ((eq type 'headline)
	   (org-babel-execute-subtree))
	  ((eq type 'src-block)
	   (org-babel-execute-src-block))
	  (t
	   (org-babel-execute-src-block-maybe))))))

(defun tviti/literate-kill-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-kill-chunk)
   (org-babel-execute-maybe)))

(defun tviti/literate-narrow-chunk ()
  (interactive)
  (cond ((tviti/polymode-active-p)
	 (polymode-toggle-chunk-narrowing))))

(defun tviti/literate-select-chunk ()
  (interactive)
  (tviti/literate-with-host
   (polymode-mark-or-extend-chunk)
   (org-babel-mark-block)))

(defun tviti/literate-toggle-inline-images ()
  (interactive)
  (tviti/literate-with-host
   nil
   (org-toggle-inline-images)))

(defun tviti/literate-fold ()
  (interactive)
  (tviti/literate-with-host
   nil
   (org-cycle)))

(defun tviti/literate-toggle-inline-latex ()
  (interactive)
  (tviti/literate-with-host
   nil
   (org-toggle-latex-fragment)))

(with-eval-after-load 'polymode
  (define-key polymode-mode-map (kbd "C-c l") #'tviti/literate))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") #'tviti/literate))

(provide 'literate-config)
