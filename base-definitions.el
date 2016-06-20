;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file contains the base definitions.

;; * Locations, to be configured
;;   - Allow prior `setq'.
;;   - `getenv' for temporary reconfig from shell environment variable.
(defcustom loc-emacs-pkg (let ((dir (getenv "loc_emacs_pkg")))
                           (if dir dir "~/c/etc/emacs/pkg"))
  "The directory that contains some single file packages.")
(defcustom loc-emacs-vc (let ((dir (getenv "loc_emacs_vc")))
                          (if dir dir "~/c/etc/emacs/vc-"))
  "The directory that contains one directory per package.")
;; Check existence of locations.
(mapc (lambda (dir) (unless (file-readable-p dir)
                      (user-error "ERR: Missing dir: %s" dir)))
      (list loc-emacs-pkg loc-emacs-vc))

;; * Lazy loading of external packages
(defun ext-pkg (dir &optional file func-or-exts-with-funcs)
  "load-path and auto-mode-alist/`autoload' for external packages.
DIR: The directory to be added to load-path, DIR contains the FILE.
FILE: The name of the file without .el to be loaded, contains the
implementation(s) of the function(s) in FUNC-OR-EXTS-WITH-FUNCS.
FUNC-OR-EXTS-WITH-FUNCS: Just a function or in a cons an optional file
extension regexp and its function or optionally several cons in a list."
  ;; - History
  ;;   - 2016-06-15 New
  ;;   - 2016-06-16 Factor out `auto-stuff'
  (when (file-readable-p dir)
    (add-to-list 'load-path dir)
    (when (and file func-or-exts-with-funcs)
      (auto-stuff file func-or-exts-with-funcs))))

(defun auto-stuff (file func-or-exts-with-funcs)
  "auto-mode-alist and `autoload' for external packages.
`auto-stuff' should not be used outside of `ext-pkg' because the
~(add-to-list 'load-path dir)~ of `ext-pkg' is a prerequisite for
`auto-stuff' and therefore `ext-pkg' has to be used instead."
  ;; - Note that this solution does not mutate func-or-exts-with-funcs.
  ;; - History
  ;;   - 2016-06-16 Factored out of `ext-pkg', use polymorphic recursion
  (if (not (consp func-or-exts-with-funcs))
      ;; Polymorphic recursion for parameter normalization, outer level:
      ;; ~'func~ -> ~'(nil . func)~.
      (auto-stuff file (cons nil func-or-exts-with-funcs))
    (if (not (consp (car func-or-exts-with-funcs)))
        ;; Polymorphic recursion for parameter normalization, inner level:
        ;; ~'([...] . func)~ -> ~'(([...] . func))~.
        (auto-stuff file (list func-or-exts-with-funcs))
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

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
