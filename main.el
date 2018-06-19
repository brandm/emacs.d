;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This is the main file to prepare and load the other setup files.

;; * Base setup
;;   - As early as possible the init of t0, then the advice `provide' and
;;     then the other stuff required in case of a failure during this setup.
;;     User errors (`f-check-directories') at last.
;;   - Want 1) to see the *messages* buffer on top and 2) no scratch buffer.
(message "INF 0.000: Base setup...")
(load-file (concat (file-name-directory load-file-name)
                   "base-definitions.el"))
(defadvice provide (after advice-provide-after)
  (funcall v-provide-advice feature))
(ad-activate #'provide)
(ignore-errors (kill-buffer "*scratch*")) ; Allow it to be already killed
(when (eq system-type 'darwin)
  (setq-default
   ;; | Keyboard key   | Emacs action | Comment                         |
   ns-command-modifier   'control
   ns-option-modifier    'none        ; Leave the OS insert AltGr-chars
   ns-control-modifier   'meta))
(f-check-directories '(v-f v-d))
(f-msg "INF" "Base setup...done")

;; * Non-lazy load of the features used during lazy load
;;   - As a side effect of calling some of the functions instrumented with
;;     autoload and used later during lazy load do load as few features as
;;     possible now.
(kbd "")
(cl-delete-if nil nil)
(cl-every nil nil)
(cl-loop repeat 0)
(pcase nil)

;; * Lazy load features
(f-msg "INF" "Lazy load...")
;; If a `provide' occurs then stop with an error.
(let ((v-provide-advice #'load-err))
  (dolist (file '("lazy-load-external.el"
                  "lazy-load-internal-misc.el"
                  "lazy-load-internal-viper.el"))
    (load-file (concat (file-name-directory load-file-name) file))))
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
