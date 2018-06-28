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
(pcase-dolist (`(,key ,func) '(("C-c c" event-apply-control-modifier)
                               ("C-c m" event-apply-meta-modifier)
                               ("C-c s" event-apply-shift-modifier)))
  (define-key key-translation-map (kbd key) func))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
