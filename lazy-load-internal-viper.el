;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the internal package Viper
;;     mode.

;; * Viper mode (minor mode)
;;   - ESC and Emacs Meta prefix (see also keyboard.org):
;;     |     | insert-state        | other states                  |
;;     |-----+---------------------+-------------------------------|
;;     | ESC | like vi: quit state | like vi: do nothing, ~(ding)~ |
;;     | C-[ | like vi: quit state | like Emacs: Meta as a prefix  |
;;     | M-  | Meta as a modifier  | Meta as a modifier            |
;;   - Tests to be made after changes: Go through the description above,
;;     test the windowed and the terminal Emacs.
;;   - History
;;     - 2007-08-21 New
;;     - 2008-03-08 Off-topic: Switch from qwertz to Colemak keyboard layout
;;     - 2008-11-17 quit viper insert mode with C-c
;;     - 2008-04-07 Vim nnoremap p<->k and n<->j for Colemak keyboard layout
;;     - 2013-06-28 ESC or C-[ to quit insert-state (was customized C-c)
(setq-default
 viper-mode t
 viper-inhibit-startup-message t
 viper-expert-level 3)
(feature 'viper 'setup-feature-viper)

(defun setup-feature-viper ()
  (msg "INF" "`setup-feature-viper'")
  ;; After the feature "viper" has been provided to make
  ;; viper-want-ctl-h-help keep the value t.
  (setq-default viper-want-ctl-h-help t)
  ;; Revert some Viper mode mappings.
  (mapc (lambda (key) (define-key viper-vi-basic-map (kbd key) nil))
        '("C-u"       ; Keep `universal-argument'. Was `viper-scroll-down'.
                      ; In Viper mode use "C-b C-d" instead of C-u.
          "C-c /"     ; Keep `org-sparse-tree' in Org mode. Was
                      ; `viper-toggle-search-style'.
          "C-c M-n")) ; Keep `cider-repl-set-ns' in Cider mode. Was
                      ; `viper-next-destructive-command'.
  (define-key viper-vi-basic-map [remap open-line] 'open-line-for-viper)
  (define-key viper-vi-global-user-map "gg" 'beginning-of-buffer)
  (define-key viper-insert-basic-map (kbd "C-[") 'viper-change-state-to-vi))

(defun open-line-for-viper ()
  "For Viper mode: Disable `open-line' except in Org table."
  ;; - History
  ;;   - 2014-11-27 New
  (interactive)
  (if (and (eq major-mode 'org-mode) (org-at-table-p) org-special-ctrl-o)
      ;; From `org-open-line' that remaps the keys for `open-line'.
      (org-table-insert-row)
    (user-error "ERR: Get used to \"O\" (S-o) to behave vi-compatible.")))

;; * Colemak keyboard layout remapping
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
(defvar kbd-layout nil
  "The keyboard layout currently in use.
Use ~(setq kbd-layout \"Colemak\")~ before loading this file if
you use the Colemak keyboard layout.")

;; Keep separate from `setup-feature-viper' defined in the other section.
(when (equal kbd-layout "Colemak")
  (feature 'viper 'pk-nj-for-viper-swap))

(defun pk-nj-for-viper-swap ()
  "Vim nnoremap p<->k and n<->j for the Colemak keyboard layout."
  (interactive)
  (define-key viper-vi-basic-map "p" 'viper-previous-line) ; Was "k"
  (define-key viper-vi-basic-map "k" 'viper-put-back)      ; Was "p"
  (define-key viper-vi-basic-map "n" 'viper-next-line)     ; Was "j"
  (define-key viper-vi-basic-map "j" 'viper-search-next))  ; Was "n"

(defun pk-nj-for-viper-reset ()
  "Reset of `pk-nj-for-viper-swap'."
  (interactive)
  (define-key viper-vi-basic-map "k" 'viper-previous-line)
  (define-key viper-vi-basic-map "p" 'viper-put-back)
  (define-key viper-vi-basic-map "j" 'viper-next-line)
  (define-key viper-vi-basic-map "n" 'viper-search-next))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
