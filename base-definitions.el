;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file contains the base definitions.

;; * Lazy load features
(defvar v-f nil
  "Directory for single-file-packages.
nil allowed for quick start like `emacs -Q -l emacs.d/main.el'.")

(defvar v-d nil
  "Directory with one subdirectory per package.
nil allowed for quick start like `emacs -Q -l emacs.d/main.el'.")

(defvar v-s nil
  "Use special key bindings.")

(defun f-file-readable-p (directory &optional subdirectory)
  "Return path if DIRECTORY with SUBDIRECTORY exists."
  ;; History:
  ;; - 2017-09-07 Factored out from `f-load-path-add'
  (and directory (let ((path (concat (file-name-as-directory
                                      (expand-file-name directory))
                                     subdirectory)))
                   (and (file-readable-p path) path))))

(defun f-load-path-add (directory &optional subdirectory)
  "If DIRECTORY with SUBDIRECTORY exist add it to `load-path'.
Return non-nil if successful. Used for internal packages that
have to be redirected to an external directory or for external
packages."
  ;; History:
  ;; - 2016-06-15 Create
  ;; - 2016-06-16 Factor out `f-auto-loads'
  ;; - 2017-09-07 Factor out `f-file-readable-p'
  (let ((path (f-file-readable-p directory subdirectory)))
    (when path (add-to-list 'load-path path))))

(defun f-auto-loads (file &rest func-or-ext-with-func)
  "`autoload' and `auto-mode-alist' for packages.

FILE: The name of the file without .el to be loaded, contains the
implementation of the function(s) in FUNC-OR-EXT-WITH-FUNC.
FUNC-OR-EXT-WITH-FUNC: Just a function or a cons with a file
extension regexp and a function.

For external packages use `f-auto-loads' conditionally with
`f-load-path-add' because `load-path' is a prerequisite:
`(when (f-load-path-add \"DIRECTORY\")
   (f-auto-loads [...]))'."
  ;; History:
  ;; - 2016-06-16 Factored out from `f-load-path-add'
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

FEATURE is like in `(provide \\='feature)'. Unlike
`eval-after-load' no other type than feature for FEATURE. When
the `provide' and the file name of the feature do not match
FEATURE is a list with the feature and file name used by
`f-require-lazy-features' as the first two arguments for
`require'.

For the case of temporary workarounds or trials with a non-lazy
load of a feature before calling `f-feature' it should be called
only after the definition of SETUP-FEATURE-FUNCTION. And only
after the last steps of the feature setup so that the in this
case immediate call of SETUP-FEATURE-FUNCTION is still the very
last step of the feature setup."
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

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
