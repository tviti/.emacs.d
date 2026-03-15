;;; test-config.el --- ERT tests for Emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Validates the Emacs configuration to catch errors before running
;; interactively.  Useful when bootstrapping on a new machine.
;;
;; Usage:
;;   make test        Run all tests (requires installed packages)
;;   make test-pre    Run pre-install tests only (no packages needed)
;;   make test-post   Run post-install tests only
;;   make bootstrap   Install packages, then run all tests

;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; ----------------------------------------------------------------
;;;; Infrastructure
;;;; ----------------------------------------------------------------

(defvar tviti/test-emacs-dir
  (file-name-as-directory
   (expand-file-name
    ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the .emacs.d repository.")

(defvar tviti/test-config-dir
  (expand-file-name "config/" tviti/test-emacs-dir))

(defvar tviti/test-init-file
  (expand-file-name "init.el" tviti/test-emacs-dir))

;; Point user-emacs-directory at our repo so package-initialize works
(setq user-emacs-directory tviti/test-emacs-dir)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Mirror init.el load-path additions
(add-to-list 'load-path tviti/test-config-dir)
(add-to-list 'load-path (expand-file-name "config/nix-flymake" tviti/test-emacs-dir))

;;;; ----------------------------------------------------------------
;;;; Helpers
;;;; ----------------------------------------------------------------

(defun tviti/test-config-files ()
  "Return list of config .el files, excluding backups and encrypted files."
  (cl-remove-if
   (lambda (f) (string-match-p "\\(?:#\\|~$\\|flycheck_\\|\\.gpg$\\)" f))
   (directory-files tviti/test-config-dir t "\\.el$")))

(defun tviti/test-active-config-files ()
  "Return config files that are actively loaded by init.el."
  (let ((init-reqs (tviti/test-file-requires tviti/test-init-file)))
    (cl-remove-if-not
     (lambda (f) (memq (intern (file-name-base f)) init-reqs))
     (tviti/test-config-files))))

(defun tviti/test-read-forms (file)
  "Read all top-level S-expressions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun tviti/test-quoted-symbol (form)
  "Extract the symbol from FORM if it is (quote SYMBOL), else nil."
  (when (and (listp form)
             (eq (car form) 'quote)
             (symbolp (cadr form)))
    (cadr form)))

(defun tviti/test-file-provides (file)
  "Return the feature symbol that FILE provides, or nil."
  (cl-loop for form in (tviti/test-read-forms file)
           when (and (listp form) (eq (car form) 'provide))
           return (tviti/test-quoted-symbol (cadr form))))

(defun tviti/test-file-requires (file)
  "Return list of (require \\='FEATURE) symbols from FILE."
  (cl-loop for form in (tviti/test-read-forms file)
           when (and (listp form) (eq (car form) 'require))
           for sym = (tviti/test-quoted-symbol (cadr form))
           when sym collect sym))

(defun tviti/test-collect-hooks (forms)
  "Collect hook symbols from add-hook calls in FORMS.
Recurses into common wrapper forms."
  (let (hooks)
    (dolist (form forms (nreverse hooks))
      (when (listp form)
        (cond
         ((eq (car form) 'add-hook)
          (let ((sym (tviti/test-quoted-symbol (cadr form))))
            (when sym (push sym hooks))))
         ((memq (car form) '(with-eval-after-load eval-after-load
                              progn when unless if let let*))
          (setq hooks (nconc hooks
                             (tviti/test-collect-hooks (cdr form))))))))))

(defun tviti/test-collect-eval-after-load-features (forms)
  "Collect feature symbols from with-eval-after-load in FORMS."
  (let (features)
    (dolist (form forms (nreverse features))
      (when (listp form)
        (cond
         ((memq (car form) '(with-eval-after-load eval-after-load))
          (let ((sym (tviti/test-quoted-symbol (cadr form))))
            (when sym (push sym features))))
         ((memq (car form) '(progn when unless if let let*))
          (setq features
                (nconc features
                       (tviti/test-collect-eval-after-load-features
                        (cdr form))))))))))

(defun tviti/test-init-use-packages ()
  "Return list of package symbols from use-package forms in init.el."
  (cl-loop for form in (tviti/test-read-forms tviti/test-init-file)
           when (and (listp form)
                     (eq (car form) 'use-package)
                     (symbolp (cadr form)))
           collect (cadr form)))

;;;; ================================================================
;;;; SYNTAX — all files parse as valid S-expressions
;;;; ================================================================

(ert-deftest test-syntax/init-el ()
  "init.el contains valid S-expressions."
  (should (tviti/test-read-forms tviti/test-init-file)))

(dolist (file (tviti/test-config-files))
  (let ((base (file-name-base file)))
    (eval
     `(ert-deftest ,(intern (format "test-syntax/%s" base)) ()
        ,(format "config/%s.el contains valid S-expressions." base)
        (should (tviti/test-read-forms ,file)))
     t)))

;;;; ================================================================
;;;; PROVIDE — every config file provides its feature
;;;; ================================================================

(ert-deftest test-provide/all-configs-have-provide ()
  "Every config .el file contains a (provide \\='feature) form."
  (dolist (file (tviti/test-config-files))
    (should (tviti/test-file-provides file))))

(ert-deftest test-provide/feature-matches-filename ()
  "Each config file provides a feature matching its filename."
  (dolist (file (tviti/test-config-files))
    (let ((feature (tviti/test-file-provides file))
          (expected (intern (file-name-base file))))
      (when feature
        (should (eq feature expected))))))

;;;; ================================================================
;;;; REQUIRE CHAIN — init.el requires map to real files
;;;; ================================================================

(ert-deftest test-requires/init-config-files-exist ()
  "Every config feature required by init.el has a file in config/."
  (let ((config-features (mapcar (lambda (f) (intern (file-name-base f)))
                                 (tviti/test-config-files))))
    (dolist (req (tviti/test-file-requires tviti/test-init-file))
      (when (memq req config-features)
        (should (file-exists-p
                 (expand-file-name (format "%s.el" req)
                                   tviti/test-config-dir)))))))

;;;; ================================================================
;;;; STRUCTURE — static analysis
;;;; ================================================================

(ert-deftest test-structure/hook-names-valid ()
  "All add-hook targets end in -hook or -functions."
  (dolist (file (cons tviti/test-init-file (tviti/test-config-files)))
    (dolist (hook (tviti/test-collect-hooks (tviti/test-read-forms file)))
      (let ((name (symbol-name hook)))
        (should (or (string-suffix-p "-hook" name)
                    (string-suffix-p "-functions" name)))))))

(ert-deftest test-structure/no-circular-config-deps ()
  "Config files have no circular require dependencies."
  (let ((dep-graph (make-hash-table :test 'eq))
        (config-features (mapcar (lambda (f) (intern (file-name-base f)))
                                 (tviti/test-config-files))))
    (dolist (file (tviti/test-config-files))
      (let* ((feature (intern (file-name-base file)))
             (deps (cl-intersection (tviti/test-file-requires file)
                                    config-features)))
        (puthash feature deps dep-graph)))
    (let ((visiting (make-hash-table :test 'eq))
          (visited (make-hash-table :test 'eq))
          (cycle nil)
          (dfs nil))
      (setq dfs
            (lambda (node path)
              (cond
               ((gethash node visiting)
                (setq cycle (reverse (cons node path))))
               ((not (gethash node visited))
                (puthash node t visiting)
                (dolist (dep (gethash node dep-graph))
                  (unless cycle (funcall dfs dep (cons node path))))
                (remhash node visiting)
                (puthash node t visited)))))
      (dolist (feature config-features)
        (unless cycle (funcall dfs feature nil)))
      (should-not cycle))))

;;;; ================================================================
;;;; LOAD ORDER — critical dependency ordering in init.el
;;;; ================================================================

(ert-deftest test-load-order/linter-before-lsp ()
  "linter-config is required before lsp-config in init.el."
  (let* ((reqs (tviti/test-file-requires tviti/test-init-file))
         (linter-pos (cl-position 'linter-config reqs))
         (lsp-pos (cl-position 'lsp-config reqs)))
    (should linter-pos)
    (should lsp-pos)
    (should (< linter-pos lsp-pos))))

(ert-deftest test-load-order/user-globals-before-functions ()
  "user-globals is required before user-functions."
  (let* ((reqs (tviti/test-file-requires tviti/test-init-file))
         (g-pos (cl-position 'user-globals reqs))
         (f-pos (cl-position 'user-functions reqs)))
    (should g-pos)
    (should f-pos)
    (should (< g-pos f-pos))))

(ert-deftest test-load-order/user-functions-before-consumers ()
  "user-functions loads before evil-config and global-keys."
  (let* ((reqs (tviti/test-file-requires tviti/test-init-file))
         (f-pos (cl-position 'user-functions reqs))
         (evil-pos (cl-position 'evil-config reqs))
         (keys-pos (cl-position 'global-keys reqs)))
    (should f-pos)
    (should evil-pos)
    (should keys-pos)
    (should (< f-pos evil-pos))
    (should (< f-pos keys-pos))))

;;;; ================================================================
;;;; BYTE-COMPILATION (post-install)
;;;; ================================================================

;; init.el is not byte-compiled: it's a sequence of use-package/require
;; calls whose side effects (package loading, interactive prompts) make
;; byte-compilation impractical.  The subprocess test-load/init-el test
;; validates it instead.

(dolist (file (tviti/test-active-config-files))
  (let ((base (file-name-base file)))
    (eval
     `(ert-deftest ,(intern (format "test-bytecomp/%s" base)) ()
        ,(format "config/%s.el byte-compiles without errors." base)
        :tags '(:post-install)
        (let ((elc (byte-compile-dest-file ,file)))
          (unwind-protect
              (should (byte-compile-file ,file))
            (when (file-exists-p elc) (delete-file elc)))))
     t)))

;;;; ================================================================
;;;; FULL INIT LOAD (post-install, subprocess)
;;;; ================================================================

(ert-deftest test-load/init-el ()
  "init.el loads without error in a clean Emacs subprocess."
  :tags '(:post-install)
  (let ((output-buf (generate-new-buffer " *test-init-load*")))
    (unwind-protect
        (let ((exit-code
               (call-process
                (expand-file-name invocation-name invocation-directory)
                nil output-buf nil
                "--batch" "--no-site-file"
                "--eval" (format "(setq user-emacs-directory %S)"
                                 tviti/test-emacs-dir)
                ;; Use a unique server name to avoid conflicting with
                ;; the running Emacs server.
                "--eval" (concat "(setq server-name"
                                 " (format \"test-%d\" (emacs-pid)))")
                ;; pdf-tools-install prompts interactively; defer it
                ;; so batch mode can complete.
                "--eval" "(advice-add 'pdf-tools-install :override #'ignore)"
                "--load" tviti/test-init-file
                "--eval" "(kill-emacs 0)")))
          (unless (eq exit-code 0)
            (ert-fail
             (format "init.el failed to load (exit %d):\n%s"
                     exit-code
                     (with-current-buffer output-buf
                       (buffer-string))))))
      (kill-buffer output-buf))))

;;;; ================================================================
;;;; SYMBOL VERIFICATION (post-install)
;;;; ================================================================

(ert-deftest test-symbols/custom-functions-defined ()
  "Key tviti/* functions are fboundp after loading config."
  :tags '(:post-install)
  (require 'user-globals)
  (require 'user-functions)
  (require 'linter-config)
  (require 'ruler-mode-config)
  (require 'project-config)
  (dolist (fn '(tviti/kill-all-buffers
                tviti/copy-buffer-name
                tviti/copy-buffer-directory
                tviti/linter-on
                tviti/ruler-match-theme))
    (should (fboundp fn))))

(ert-deftest test-symbols/custom-variables-bound ()
  "Key tviti/* variables are boundp after loading config."
  :tags '(:post-install)
  (require 'user-globals)
  (require 'linter-config)
  (dolist (var '(tviti/sync-dir
                 tviti/evil-leader
                 tviti/linter))
    (should (boundp var))))

;;;; ================================================================
;;;; PACKAGE VERIFICATION (post-install)
;;;; ================================================================

(ert-deftest test-packages/use-package-pkgs-available ()
  "All use-package declarations in init.el reference available packages."
  :tags '(:post-install)
  (dolist (pkg (tviti/test-init-use-packages))
    (should (or (package-installed-p pkg)
                (locate-library (symbol-name pkg))))))

;;;; ================================================================
;;;; EVAL-AFTER-LOAD FEATURES (post-install)
;;;; ================================================================

(ert-deftest test-structure/eval-after-load-features-locatable ()
  "All with-eval-after-load feature names are locatable.
Features referenced only inside with-eval-after-load are deferred, so
missing ones are tolerated with a warning rather than a hard failure."
  :tags '(:post-install)
  (let (missing)
    (dolist (file (cons tviti/test-init-file (tviti/test-active-config-files)))
      (dolist (feat (tviti/test-collect-eval-after-load-features
                     (tviti/test-read-forms file)))
        (unless (or (locate-library (symbol-name feat))
                    (featurep feat))
          (push (cons feat (file-name-nondirectory file)) missing))))
    (when missing
      (ert-skip
       (format "Optional features not installed: %s"
               (mapconcat (lambda (m) (format "%s (%s)" (car m) (cdr m)))
                          missing ", "))))))

;;; test-config.el ends here
