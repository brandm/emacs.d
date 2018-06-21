;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This is the main file to prepare and load the other setup files.

;; * Initial setup
;;   - First the init of t0, then advice `provide' to catch as much of
;;     loading as possible, then not earlier the first explicit log and then
;;     the basic accessibility for a better interaction in case of a failure
;;     in the more complicated rest following.
;;   - Want 1) to see the *messages* buffer on top and 2) no scratch buffer.
;; ** Timing
(let ((t0 (float-time)))
  (defun f-msg (level-str format-str &rest args)
    "`message' with time since start."
    (message "%s %.3f: %s"
             level-str
             (- (float-time) t0)
             (apply #'format format-str args))))

;; ** Implicit logging of feature loading
(defun load-err (feature)
  (funcall (if t #'error #'message) ; t: normal, nil: debug
           "ERR: Too early loaded feature %S" feature))
(defun load-inf (feature)
  (f-msg "INF" "Loaded feature %S" feature))
(defvar v-provide-advice #'load-inf)
(defadvice provide (after advice-provide-after)
  (funcall v-provide-advice feature))
(ad-activate #'provide)

;; ** First explicit log
(f-msg "INF" "Initial setup...ongoing")

;; ** Basic accessibility
(when (and (display-graphic-p) (eq system-type 'darwin))
  (setq-default
   ;; | Keyboard key   | Emacs action | Comment                         |
   ns-command-modifier   'control
   ns-option-modifier    'none        ; Leave the OS insert AltGr-chars
   ns-control-modifier   'meta))
(ignore-errors (kill-buffer "*scratch*")) ; Allow it to be already killed

;; ** Load (non-lazy) of the features used during setup
;;    - As a side effect of calling some of the functions instrumented with
;;      autoload and used later during lazy load do load as few features as
;;      possible now.
;;    - Before changing `load-path' as external features are not allowed
;;      during setup.
(kbd "")
(cl-delete-if nil nil)
(cl-every nil nil)
(cl-loop repeat 0)
(pcase nil)

;; ** Initial setup finish
(setq v-provide-advice #'load-err)
(f-msg "INF" "Initial setup...done")

;; * `load-path' and `Info-directory-list'
(defun f-check-directories (symbol-list)
  "Check the directories to be set on the Emacs command line."
  (dolist (symbol symbol-list)
    (let ((value (symbol-value symbol)))
      (if value
          (unless (file-readable-p value)
            (user-error "ERR: `%s' specifies a missing directory %s"
                        symbol value))
        (f-msg "INF" "`%s' not specified" symbol)))))
(f-check-directories '(v-f v-d))

;; * Lazy load features
(f-msg "INF" "Lazy load...")
(dolist (file '("base-definitions.el"
                "lazy-load-external.el"
                "lazy-load-internal-misc.el"
                "lazy-load-internal-viper.el"))
  (load-file (concat (file-name-directory load-file-name) file)))
(setq v-provide-advice #'load-inf)
(f-msg "INF" "Lazy load...done")

;; * Non-lazy load features
;; `with-temp-buffer' to make viper-want-ctl-h-help keep the value t
;; although the scratch buffer has been killed before ~(require 'viper)~.
(with-temp-buffer (require 'viper)) ; How to lazy load without `require'?

;; * Succeeded
(f-msg "INF" "#### Loaded file %s" load-file-name)

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
