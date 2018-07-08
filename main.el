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
;;   - Want to see the *messages* buffer on top and no scratch buffer.
;; ** Timing
(let ((t0 (float-time)))
  (defun f-msg (level-str &optional format-str &rest args)
    "`message' with time since start."
    (if format-str
        (message "%s %.3f: %s"
                 level-str
                 (- (float-time) t0)
                 (apply #'format format-str args))
      (message "%s %.3f" level-str (- (float-time) t0)))))

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
(f-msg "INF" "Initial setup...continued")

;; ** Basic accessibility
(when (and (display-graphic-p) (eq system-type 'darwin))
  (setq-default
   ;; Mapping of keyboard keys to Emacs action. From the outer to the inner
   ;; keys. Commented out where it is the default behavior, with the same or
   ;; an equivalent value.
   ;; . ns-control-modifier       'control
   ;; . ns-right-control-modifier 'control
   ;; . ns-alternate-modifier     'meta
   ns-right-alternate-modifier    'none      ; Let the OS do AltGr-chars
   ns-command-modifier            'control   ; Alternative for thumb
   ns-right-command-modifier      'control)) ; Alternative for thumb
(ignore-errors (kill-buffer "*scratch*")) ; Allow it to be already killed

;; ** Load (non-lazy) of the features used during setup
;;    - As a side effect of calling some of the functions instrumented with
;;      autoload and used later during lazy load do load as few features as
;;      possible now.
;;    - Before changing `load-path'. External features from a non-default
;;      `load-path' during setup would not be a good idea anyway.
(kbd "")
(cl-delete-if nil nil)
(cl-every nil nil)
(cl-loop repeat 0)
(make-glyph-code nil)
(pcase nil)

;; ** Initial setup finish
(f-msg "INF" "Initial setup...done")

;; * Base definitions
(let ((v-provide-advice #'load-err))
  (load-file (concat (file-name-directory load-file-name)
                     "base-definitions.el"))
  (f-msg "INF"))

;; * Check and report `v-f' and `v-d'
(defun f-check-directories (symbol-list)
  "Check the directories to be set on the Emacs command line."
  (dolist (symbol symbol-list)
    (let ((value (symbol-value symbol)))
      (if value
          (unless (file-readable-p value)
            (user-error "ERR: `%s' specifies a missing directory %s"
                        symbol value))
        (f-msg "INF" "`%s' not specified" symbol)))))

(let ((v-provide-advice #'load-err))
  (f-check-directories '(v-f v-d)))

;; * Load (non-lazy) features
(load-file (concat (file-name-directory load-file-name) "load-external.el"))
(f-msg "INF")

;; * Lazy load features
(let ((v-provide-advice #'load-err))
  (dolist (file '("misc.el"
                  "lazy-load-external.el"
                  "lazy-load-internal-misc.el"
                  "lazy-load-internal-viper.el"))
    (load-file (concat (file-name-directory load-file-name) file))
    (f-msg "INF")))

;; * Load (non-lazy) features
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
