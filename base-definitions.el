;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file contains the base definitions.

;; * Lazy loading of packages
(defun load-path-add (directory)
  "If DIRECTORY exists add it to load-path and return non-nil.
For internal packages that have to be redirected to an external
directory or for external packages."
  ;; - History
  ;;   - 2016-06-15 New
  ;;   - 2016-06-16 Factor out `auto-loads'
  (when (file-readable-p directory) (add-to-list 'load-path directory)))

(defun auto-loads (file &optional &rest func-or-ext-with-func)
  "`autoload' and auto-mode-alist for packages.

FILE: The name of the file without .el to be loaded, contains the
implementation of the function(s) in FUNC-OR-EXT-WITH-FUNC.
FUNC-OR-EXT-WITH-FUNC: Just a function or a cons with a file
extension regexp and a function.

For external packages use `auto-loads' conditionally with
`load-path-add' because load-path is a prerequisite:
~(when (load-path-add \"DIRECTORY\") (auto-loads [...]))~."
  ;; - History
  ;;   - 2016-06-16 Factored out of `load-path-add'
  (mapc (lambda (x)
          (autoload
            (if (consp x) (cdr x) x)
            file
            "Undocumented `autoload'."
            t)
          (when (consp x)
            (add-to-list 'auto-mode-alist x)))
        func-or-ext-with-func))

;; * Tracking and setup of lazy loaded features
(defvar lazy-features nil
  "Accumulated list of features that have been prepared for lazy loading.
Can be used to force loading at the end of the setup or later")

(defun feature (feature &optional setup-feature-function)
  "Track FEATURE and optionally lazy call of SETUP-FEATURE-FUNCTION.
The feature is the same as in the form ~(provide 'feature)~.
Unlike `eval-after-load' no other type than feature for FEATURE.
FEATURE has to be a list of parameters to be passed to `require'
instead of just the feature when the `provide' and the file name
of the feature do not match."
  (add-to-list 'lazy-features feature t)
  (with-eval-after-load (if (consp feature) (car feature) feature)
    (when setup-feature-function (funcall setup-feature-function))))

(defun require-lazy-features ()
  "Force `require' of features that have been prepared with `feature'."
  (interactive)
  (msg "INF" "#### Requiring lazy loaded features...")
  (mapc (lambda (feature)
          (if (consp feature)
              ;; `provide' and file name of the feature do not match.
              (apply #'require feature)
            (require feature)))
        lazy-features)
  (msg "INF" "#### Requiring lazy loaded features...done"))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
