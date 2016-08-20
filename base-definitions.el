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

(defun auto-loads (file func-or-exts-with-funcs)
  "auto-mode-alist and `autoload' for packages.

FILE: The name of the file without .el to be loaded, contains the
implementation(s) of the function(s) in FUNC-OR-EXTS-WITH-FUNCS.
FUNC-OR-EXTS-WITH-FUNCS: Just a function or in a cons an optional
file extension regexp and its function or optionally several cons
in a list.

Use ~(when (load-path-add DIRECTORY) (auto-loads FILE FUNC))~ for
external packages because load-path is a prerequisite."
  ;; - Note that this solution does not mutate func-or-exts-with-funcs.
  ;; - History
  ;;   - 2016-06-16 Factored out of `load-path-add', polymorphic recursion
  (cond ((not (consp func-or-exts-with-funcs))
         ;; Polymorphic recursion for parameter normalization, outer level:
         ;; ~'func~ -> ~'(nil . func)~.
         (auto-loads file (cons nil func-or-exts-with-funcs)))
        ((not (consp (car func-or-exts-with-funcs)))
         ;; Polymorphic recursion for parameter normalization, inner level:
         ;; ~'([...] . func)~ -> ~'(([...] . func))~.
         (auto-loads file (list func-or-exts-with-funcs)))
        (t
         ;; Parameter normalization done, ready for the actual work.
         (mapc (lambda (extension-and-function)
                 (autoload
                   (cdr extension-and-function)
                   file
                   "Undocumented `autoload'."
                   t)
                 (when (car extension-and-function)
                   (add-to-list 'auto-mode-alist extension-and-function)))
               func-or-exts-with-funcs))))

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
