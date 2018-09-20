;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file is for miscellaneous.

;; * Keyboard
;;   - History:
;;     - 2018-06-24 Create
;; ** Misc
(setq echo-keystrokes 0.01)
(pcase-dolist (`(,key ,func) '(("C-c c" event-apply-control-modifier)
                               ("C-c m" event-apply-meta-modifier)
                               ("C-c s" event-apply-shift-modifier)))
  (define-key key-translation-map (kbd key) func))

(defun f-event-apply-modifier-advice (event symbol lshiftby prefix)
  "Fix C0 control character range of `event-apply-modifier'.
See
http://lists.gnu.org/archive/html/help-gnu-emacs/2018-07/msg00073.html
http://lists.gnu.org/archive/html/help-gnu-emacs/2018-08/msg00077.html
http://git.savannah.gnu.org/cgit/emacs.git/commit/lisp/simple.el?id=8c8bf7d"
  (when (and (numberp event) (eq symbol 'control) (<= 64 (upcase event) 95))
    (- (upcase event) 64)))

(when (version< emacs-version "27")
  (advice-add
   'event-apply-modifier :before-until #'f-event-apply-modifier-advice))

;; ** Insert non-ASCII characters
;;    - Could also be:
;;      - A single loop that calls a macro that defines each function and
;;        binds it.
;;      - A single function with an interactive argument bound to `C-c w'.
;;      - A hydra.
;; Diaeresis for some vowels.
(defun f-insert-char-U+00C4 () (interactive) (f-insert-char ?\u00C4))
(defun f-insert-char-U+00D6 () (interactive) (f-insert-char ?\u00D6))
(defun f-insert-char-U+00DC () (interactive) (f-insert-char ?\u00DC))
(defun f-insert-char-U+00E4 () (interactive) (f-insert-char ?\u00E4))
(defun f-insert-char-U+00F6 () (interactive) (f-insert-char ?\u00F6))
(defun f-insert-char-U+00FC () (interactive) (f-insert-char ?\u00FC))

(defun f-insert-char (char)
  "Insert CHAR and report unicode code to finally learn it."
  (insert-char char)
  (message "INF: Inserted U+%04X" char))

(pcase-dolist
    (`(,key ,func)
     '(;; Diaeresis for some vowels.
       ("C-c w A" f-insert-char-U+00C4)
       ("C-c w O" f-insert-char-U+00D6)
       ("C-c w U" f-insert-char-U+00DC)
       ("C-c w a" f-insert-char-U+00E4)
       ("C-c w o" f-insert-char-U+00F6)
       ("C-c w u" f-insert-char-U+00FC)))
  (global-set-key (kbd key) func))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
