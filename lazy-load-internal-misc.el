;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the internal packages
;;     except Viper mode.

;; * C mode and C++ mode (major modes)
(defun f-setup-buffer-c-or-c++ ()
  (f-msg "INF" "`f-setup-buffer-c-or-c++'")
  ;; According to
  ;; http://lists.gnu.org/archive/html/emacs-devel/2005-12/msg00287.html
  ;; `c-set-style' has to be in setup buffer, not in setup feature.
  (c-set-style "Stroustrup"))

(defun f-setup-feature-c-or-c++ ()
  (f-msg "INF" "`f-setup-feature-c-or-c++'")
  (add-hook 'c-mode-hook   'f-setup-buffer-c-or-c++)
  (add-hook 'c++-mode-hook 'f-setup-buffer-c-or-c++))

(f-feature 'cc-mode 'f-setup-feature-c-or-c++)

;; * Diff mode (major mode)
(defvar diff-mode-map)

(defun f-setup-feature-diff-mode ()
  (f-msg "INF" "`f-setup-feature-diff-mode'")
  ;; Set foreground colors to those used by git for colored diff. It makes
  ;; it much clearer to ediff two diff files.
  (custom-set-faces '(diff-added   ((t (:foreground "forest green"))) 'now)
                    '(diff-removed ((t (:foreground "firebrick"   ))) 'now)
                    '(diff-context ((t (:foreground "black"       ))) 'now))
  ;; All these were unused.
  (define-key diff-mode-map (kbd "M-<up>"   ) 'diff-file-prev)
  (define-key diff-mode-map (kbd "M-<down>" ) 'diff-file-next)
  (define-key diff-mode-map (kbd "M-<left>" ) 'diff-hunk-prev)
  (define-key diff-mode-map (kbd "M-<right>") 'diff-hunk-next))

(f-feature 'diff-mode 'f-setup-feature-diff-mode)

;; * Eldoc mode (minor mode)
(setq-default eldoc-minor-mode-string ; Also called "lighter"
              " E") ; Was " ElDoc" (leading space)
(mapc (lambda (hook) (add-hook hook 'turn-on-eldoc-mode))
      '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))

;; * isearch mode (minor mode)
;;   - Note on isearch history: First time access is via isearch-mode-map
;;     and isearch-ring-retreat, later accesses are via
;;     minibuffer-local-isearch-map and previous-history-element (already
;;     C-p)
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key. C-p
;; and C-n were exit isearch.
(define-key isearch-mode-map (kbd "C-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "C-n") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-p") nil)
(define-key isearch-mode-map (kbd "M-n") nil)
;; Use C-c to quit incremental search with this to stay at match.
(define-key isearch-mode-map (kbd "C-c") 'isearch-exit)

;; * Minibuffer
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key. C-p
;; and C-n were unused.
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "M-p") nil)
(define-key minibuffer-local-map (kbd "M-n") nil)

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
