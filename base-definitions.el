;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2017 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file contains the base definitions.

;; * Message with time since start
(let ((t0 (float-time)))
  (defun f-msg (level-str format-str &rest args)
    "`message' with time since start."
    (message "%s %.3f: %s"
             level-str
             (- (float-time) t0)
             (apply #'format format-str args))))

;; * Logging of feature loading
(defun load-err (feature)
  (funcall (if t #'error #'message) ; t: normal, nil: debug
           "ERR: Too early loaded feature %S" feature))
(defun load-inf (feature)
  (f-msg "INF" "Loaded feature %S" feature))
(defconst v-provide-advice #'load-inf)

;; * Check directories
(defun f-check-dirs (directory-list)
  "Check the directories to be set on the Emacs command line."
  (dolist (symbol directory-list)
    (let ((value (symbol-value symbol)))
      (if value
          (unless (file-readable-p value)
            (user-error "ERR: `%s' specifies a missing directory %s"
                        symbol value))
        (f-msg "INF" "`%s' not specified" symbol)))))

;; * Lazy load features
(defvar v-f nil
  "Directory for single-file-packages.
Allowed to be nil for quick start like ~emacs -Q -l emacs.d/main.el~")
(defvar v-d nil
  "Directory with one subdirectory per package.
Allowed to be nil for quick start like ~emacs -Q -l emacs.d/main.el~")

(defun f-load-path-add (directory &optional subdirectory)
  "If DIRECTORY with SUBDIRECTORY exist add it to `load-path'.
Return non-nil if successful. Used for internal packages that
have to be redirected to an external directory or for external
packages."
  ;; - History
  ;;   - 2016-06-15 Create
  ;;   - 2016-06-16 Factor out `f-auto-loads'
  (when directory
    (let ((path (concat (file-name-as-directory directory) subdirectory)))
      (when (file-readable-p path)
        (add-to-list 'load-path path)))))

(defun f-auto-loads (file &optional &rest func-or-ext-with-func)
  "`autoload' and `auto-mode-alist' for packages.

FILE: The name of the file without .el to be loaded, contains the
implementation of the function(s) in FUNC-OR-EXT-WITH-FUNC.
FUNC-OR-EXT-WITH-FUNC: Just a function or a cons with a file
extension regexp and a function.

For external packages use `f-auto-loads' conditionally with
`f-load-path-add' because `load-path' is a prerequisite:
~(when (f-load-path-add \"DIRECTORY\") (f-auto-loads [...]))~."
  ;; - History
  ;;   - 2016-06-16 Factored out from `f-load-path-add'
  (dolist (x func-or-ext-with-func)
    (autoload
      (if (consp x) (cdr x) x)
      file
      "Undocumented `autoload'."
      t)
    (when (consp x) (add-to-list 'auto-mode-alist x))))

;; * Tracking and setup of lazy loaded features
(defvar v-lazy-features nil
  "Accumulated list of features that have been prepared for lazy load.
Can be used to force loading at the end of the setup or later")

(defun f-feature (feature &optional setup-feature-function)
  "Track FEATURE and optionally lazy call of SETUP-FEATURE-FUNCTION.
The feature is the same as in the form ~(provide 'feature)~.
Unlike `eval-after-load' no other type than feature for FEATURE.
When the `provide' and the file name of the feature do not match
FEATURE is a list with the feature and file name used by
`f-require-lazy-features' as the first two arguments for
`require'."
  (add-to-list 'v-lazy-features feature t)
  (when setup-feature-function
    (with-eval-after-load (if (consp feature) (car feature) feature)
      (funcall setup-feature-function))))

(defun f-require-lazy-features ()
  "Force `require' of features prepared with `f-feature'."
  (interactive)
  (f-msg "INF" "#### Force load...")
  (dolist (feature v-lazy-features)
    (if (consp feature)
        ;; `provide' and file name of the feature do not match.
        (apply #'require feature)
      (require feature)))
  (f-msg "INF" "#### Force load...done"))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
