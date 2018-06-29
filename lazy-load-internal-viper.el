;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the internal package Viper
;;     mode.

;; * Viper mode (minor mode)
;;   - ESC and Emacs Meta prefix:
;;     |     | `insert-state'      | Other states                 |
;;     |-----+---------------------+------------------------------|
;;     | ESC | like vi: quit state | like vi: do nothing, `ding'  |
;;     | C-[ | like vi: quit state | like Emacs: Meta as a prefix |
;;     | M-  | Meta as a modifier  | Meta as a modifier           |
;;   - Tests to be made after changes: Go through the description above,
;;     test graphical and terminal Emacs.
;;   - History
;;     - 2007-08-21 Create
;;     - 2008-03-08 Off-topic: Switch from qwertz to Colemak keyboard layout
;;     - 2008-04-07 p<->k and n<->j for Colemak keyboard layout
;;     - 2008-11-17 Quit `insert-state' with `C-c'
;;     - 2013-06-28 Quit `insert-state' with `C-['
;;     - 2018-06-28 Quit `insert-state' with key chord like the common `jj'
(defun f-open-line-for-viper ()
  "For Viper mode: Disable `open-line' except in Org table."
  ;; - History
  ;;   - 2014-11-27 Create
  (interactive)
  (if (and (eq major-mode 'org-mode) (org-at-table-p) org-special-ctrl-o)
      ;; From `org-open-line' that remaps the keys for `open-line'.
      (org-table-insert-row)
    (user-error "ERR: Get used to \"O\" (S-o) to behave vi-compatible.")))

(defun f-setup-feature-viper ()
  (f-msg "INF" "`f-setup-feature-viper'")
  (setq-default viper-suppress-input-method-change-message t)
  ;; After the feature "viper" has been provided to make
  ;; `viper-want-ctl-h-help' keep the value t.
  (setq-default viper-want-ctl-h-help t)
  (if (display-graphic-p)
      (define-key viper-insert-basic-map "\e" #'viper-change-state-to-vi)
    ;; Workaround to avoid that in a terminal `ESC'
    ;; (`viper-intercept-ESC-key') in `vi-state' results in the error
    ;; "viper-ESC: Wrong type argument: stringp, [escape]" when
    ;; `viper-ESC-key' is the default `[escape]'.
    (setq-default viper-ESC-key "\e"))
  (when (fboundp #'key-chord-mode)
    (require 'key-chord)
    (key-chord-define
     viper-insert-basic-map "hh" #'viper-change-state-to-vi))
  (add-hook 'viper-insert-state-hook #'f-setup-hook-viper-insert-state)
  ;; Revert some Viper mode mappings.
  (dolist (key '("C-u"       ; Keep `universal-argument'. Was
                             ; `viper-scroll-down'. In Viper mode use "C-b
                             ; C-d" instead of C-u.
                 "C-c /"     ; Keep `org-sparse-tree' in Org mode. Was
                             ; `viper-toggle-search-style'.
                 "C-c M-n")) ; Keep `cider-repl-set-ns' in Cider mode. Was
                             ; `viper-next-destructive-command'.
    (define-key viper-vi-basic-map (kbd key) nil))
  (define-key viper-vi-global-user-map "gg" #'beginning-of-buffer)
  (define-key viper-vi-basic-map [remap open-line] #'f-open-line-for-viper)
  (when (equal v-k "co") (f-pk-nj-for-viper-swap)))

(defun f-setup-hook-viper-insert-state ()
  (when (fboundp #'key-chord-input-method)
    ;; Refresh the variable `input-method-function' as it seems to be set to
    ;; nil for example when the current window changes the buffer with for
    ;; example `bs-show' or `ibs-select'.
    (setq input-method-function #'key-chord-input-method)))

(setq-default viper-mode t
              viper-inhibit-startup-message t
              viper-expert-level 3)
(f-feature 'viper #'f-setup-feature-viper)

;; * Colemak keyboard layout remapping
;;   - Keep separate from `f-setup-feature-viper' defined in the other
;;     section.
;;   - The following background about the choice to swap p<->k and n<->j
;;     uses these abbreviations:
;;     - PREV :: Move line up _by keeping the column_.
;;     - NEXT :: Move line down _by keeping the column_.
;;   - Pure vi does not support remapping, alternatives for Colemak:
;;     - h :: Move char left: h is already the same key as with qwerty.
;;            Harder to reach when touch typing: Backspace.
;;     - l :: Move char right: Use SPC instead.
;;     - k :: PREV: Use C-p instead. The key - does not keep the column.
;;     - j :: NEXT: Use C-n instead. C-m, RET and + do not keep the column.
;;   - Vim and Emacs viper-mode remapping for Colemak: Swapping p<->k and
;;     n<->j brings NEXT and PREV to Colemak-n and Colemak-p. This way NEXT
;;     comes back to the same key as it was on qwerty-j (!) and the keys n
;;     and p are already used the same way for moving up and down in e. g.
;;     bash and Emacs, see summary below. The costs for these two swaps are
;;     that "Paste" moves from p to k and "search Next" moves from n to j.
;;   - Summary of line movement keys in some tools:
;;     - C-p / C-n :: bash history of command line.
;;     - M-p / M-n :: Emacs history of minibuffer (find-file, grep-find,
;;                    compile, find-tag and more).
;;     -   p /   n :: Emacs dired-mode, buffer-list and more.
;;     - C-p / C-n :: Emacs PREV/NEXT in buffer editing without viper-mode.
;;     - C-p / C-n :: Emacs PREV/NEXT in buffer editing with viper-mode if
;;                    remapped for Colemak or not.
;;     -   p /   n :: Emacs PREV/NEXT in buffer editing with viper-mode if
;;                    remapped for Colemak.
;;     - C-p / C-n :: Vim PREV/NEXT alternative keys, first of all if p/n
;;                    not remapped for Colemak.
;;     -   p /   n :: Vim PREV/NEXT common keys if remapped for Colemak.
;;     - C-p / C-n :: vi PREV/NEXT alternative keys, first of all if p/n
;;                    not remapped for Colemak.
;;     -   k /   j :: vi PREV/NEXT common keys (not remappable without
;;                    recompilation).
;;     - C-p / C-n :: GNU Screen selection in list of terminal sessions
;;                    (C-a ") alternative keys.
;;     -   k /   j :: GNU Screen selection in list of terminal sessions
;;                    (C-a ") common keys.
;;     -   k /   j :: /usr/bin/less common keys.
;;     -   y /   e :: /usr/bin/less alternative keys (easier for Colemak).
;;     - C-p / C-n :: /usr/bin/less alternative keys (easier for Colemak).
(defvar v-k nil
  "The keyboard layout in use.
Use `(setq v-k \"co\")' before loading this file if you use the
Colemak keyboard layout. Also convenient on the MessagEase
keyboard.")

(defun f-pk-nj-for-viper-swap ()
  "p<->k and n<->j for the Colemak keyboard layout."
  (interactive)
  (pcase-dolist (`(,key ,func) '(("p" viper-previous-line) ; Was k
                                 ("k" viper-put-back)      ; Was p
                                 ("n" viper-next-line)     ; Was j
                                 ("j" viper-search-next))) ; Was n
    (define-key viper-vi-basic-map (kbd key) func)))

(defun f-pk-nj-for-viper-reset ()
  "Reset of `f-pk-nj-for-viper-swap'."
  (interactive)
  (pcase-dolist (`(,key ,func) '(("k" viper-previous-line)
                                 ("p" viper-put-back)
                                 ("j" viper-next-line)
                                 ("n" viper-search-next)))
    (define-key viper-vi-basic-map (kbd key) func)))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
