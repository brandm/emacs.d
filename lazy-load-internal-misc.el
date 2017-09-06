;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2017 Michael Brand <michael.ch.brand at gmail.com>
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
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'f-setup-buffer-c-or-c++)))

(f-feature 'cc-mode #'f-setup-feature-c-or-c++)

;; * Diff mode (major mode)
(defvar diff-mode-map)

(defun f-setup-feature-diff-mode ()
  (f-msg "INF" "`f-setup-feature-diff-mode'")
  ;; Set foreground colors to those used by git for colored diff. It makes
  ;; it much clearer to ediff two diff files.
  (custom-set-faces '(diff-added   ((t (:foreground "forest green"))) 'now)
                    '(diff-removed ((t (:foreground "firebrick"   ))) 'now)
                    '(diff-context ((t (:foreground "black"       ))) 'now))
  (dolist (key-func '(("M-<up>"    diff-file-prev)   ; Was unused
                      ("M-<down>"  diff-file-next)   ; Was unused
                      ("M-<left>"  diff-hunk-prev)   ; Was unused
                      ("M-<right>" diff-hunk-next))) ; Was unused
    (define-key diff-mode-map (kbd (car key-func)) (cadr key-func))))

(f-feature 'diff-mode #'f-setup-feature-diff-mode)

;; * Eldoc mode (minor mode)
(setq-default eldoc-minor-mode-string ; Also called "lighter"
              " E") ; Was " ElDoc" (leading space)
(dolist (hook '(;; Keep in sync with adding `smartparens-mode' to hooks.
                emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook #'turn-on-eldoc-mode))

;; * isearch mode (minor mode)
;;   - Note on isearch history: First time access is via isearch-mode-map
;;     and isearch-ring-retreat, later accesses are via
;;     minibuffer-local-isearch-map and previous-history-element (already
;;     C-p)
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key.
(dolist (key-func '(("C-p" isearch-ring-retreat) ; Was exit isearch
                    ("C-n" isearch-ring-advance) ; Was exit isearch
                    ("M-p" nil)
                    ("M-n" nil)))
  (define-key isearch-mode-map (kbd (car key-func)) (cadr key-func)))
;; Use C-c to quit incremental search with this to stay at match.
(define-key isearch-mode-map (kbd "C-c") #'isearch-exit)

;; * Org mode (major mode)
(defun f-setup-feature-org-main ()
  (f-msg "INF" "`f-setup-feature-org-main'")
  (setq-default
   org-cycle-global-at-bob t
   ;; Like outshine config. In Viper mode available only in Emacs state (E).
   org-use-speed-commands t)
  ;; For for example 256-color terminal and light background. The Emacs
  ;; color brightwhite is only defined in a text terminal Emacs.
  (when (and (eq 'light (frame-parameter nil 'background-mode))
             (color-defined-p "brightwhite"))
    (set-face-foreground 'org-hide "brightwhite")))

(f-feature 'org #'f-setup-feature-org-main)
(global-set-key (kbd "C-c m o") #'org-mode) ; Mnemonic Org

;; * Outline mode (major or minor mode)
;;   - History
;;     - 2017-08-27 Create.
(defun f-outline-minor-mode-toggle ()
  (interactive)
  (cond (outline-minor-mode
         (outline-minor-mode 0)) ; Turn off
        ((featurep 'outshine)
         (user-error
          (concat "ERR: outshine has already been loaded, therefore "
                  "`outline-minor-mode' can not be used without outshine "
                  "anymore. To use outshine use `f-outshine-toggle'.")))
        (t
         (outline-minor-mode)))) ; Turn on

;; Mnemonic outLine
(global-set-key (kbd "C-c m l") #'f-outline-minor-mode-toggle)

;; * Python mode (major mode)
;;   - History
;;     - 2016-09-22 Create
(defvar python-mode-map)

(defun f-setup-feature-python ()
  (f-msg "INF" "`f-setup-feature-python'")
  (define-key python-mode-map (kbd "C-c c") #'live-py-mode))

(f-feature 'python #'f-setup-feature-python)

;; * Minibuffer
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key.
(dolist (key-func '(("C-p" previous-history-element) ; Was unused
                    ("C-n" next-history-element)     ; Was unused
                    ("M-p" nil)
                    ("M-n" nil)))
  (define-key minibuffer-local-map (kbd (car key-func)) (cadr key-func)))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
