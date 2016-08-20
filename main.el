;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This is the main file to prepare and load the other setup files.

;; * Base setup
;;   - First the stuff required in case of a failure during this setup.
;;   - Want 1) to see the *messages* buffer on top and 2) no scratch buffer.
(ignore-errors (kill-buffer "*scratch*")) ; Allow it to be already killed
(when (eq system-type 'darwin)
  (setq-default ns-command-modifier 'meta  ; Key "command": Emacs Meta
                ns-option-modifier 'none)) ; Key "option": Insert AltGr char

;; * Init logging
(let ((t0 (float-time)))
  (defun msg (level-str format-str &rest args)
    "`message' with time since start."
    (message "%s %.3f: %s"
             level-str
             (- (float-time) t0)
             (apply #'format format-str args))))

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

;; * Logging of feature loading
(defun load-err () (funcall (if t 'error 'message) ; t: normal, nil: debug
                            "ERR: Too early loaded feature %S" feature))
(defun load-inf () (msg "INF" "Loaded feature %S" feature))
(defconst provide-advice #'load-inf)
(defadvice provide (after advice-provide-after) (funcall provide-advice))
(ad-activate 'provide)

;; * Non-lazy feature loading for preparation
;;   - Load as few features as possible required for the setup.
(kbd "") ; As a side effect load the features required for `kbd'

;; * Lazy feature loading
;;   - If a `provide' occurs already here then stop the setup with an error.
(let ((provide-advice #'load-err))
  (mapc (lambda (file) (load-file (concat loc-emacs-vc "/emacs.d/" file)))
        '("base-definitions.el"
          "lazy-load-external.el"
          "lazy-load-internal-misc.el"
          "lazy-load-internal-viper.el")))

;; * Non-lazy feature loading for finishing
(when (< emacs-major-version 25) (require 'eldoc)) ; Default in Emacs 25.1
;; `with-temp-buffer' to make viper-want-ctl-h-help keep the value t
;; although the scratch buffer has been killed before ~(require 'viper)~.
(with-temp-buffer (require 'viper)) ; How to lazy load without `require'?

;; * Succeeded
(msg "INF" "#### Loaded file %s" load-file-name)

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
