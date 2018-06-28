;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
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
  (pcase-dolist (`(,face ,color) '((diff-added   "forest green")
                                   (diff-removed "firebrick")
                                   (diff-context "black")))
    (set-face-background face nil)
    (set-face-foreground face color))
  (pcase-dolist (`(,key ,func) '(("M-<left>"  diff-file-prev)   ; Was unused
                                 ("M-<right>" diff-file-next)   ; Was unused
                                 ("M-<up>"    diff-hunk-prev)   ; Was unused
                                 ("M-<down>"  diff-hunk-next))) ; Was unused
    (define-key diff-mode-map (kbd key) func)))

(f-feature 'diff-mode #'f-setup-feature-diff-mode)

;; * Eldoc mode (minor mode)
(setq-default eldoc-minor-mode-string ; Also called "lighter"
              " E") ; Was " ElDoc" (leading space)
(dolist (hook '(;; Keep in sync with adding `smartparens-mode' to the same
                ;; hooks.
                emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook #'eldoc-mode))

;; * isearch mode (minor mode)
;;   - Note on isearch history: First time access is via isearch-mode-map
;;     and isearch-ring-retreat, later accesses are via
;;     minibuffer-local-isearch-map and previous-history-element (already
;;     C-p)
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key.
(pcase-dolist (`( ,key  ,func)
               '(("M-p" nil)
                 ("C-p" isearch-ring-retreat) ; Was exit isearch
                 ("M-n" nil)
                 ("C-n" isearch-ring-advance))) ; Was exit isearch
  (define-key isearch-mode-map (kbd key) func))
;; Use C-c to quit incremental search with this to stay at match.
(define-key isearch-mode-map (kbd "C-c") #'isearch-exit)

;; * Org mode (major mode)
(defun f-setup-feature-org-main ()
  (f-msg "INF" "`f-setup-feature-org-main'")
  (f-msg "INF" "%s" (org-version nil t))
  (setq-default
   org-cycle-global-at-bob t
   ;; Like outshine config. In Viper mode available only in Emacs state (E).
   org-use-speed-commands t)
  ;; For example for 256-color terminal and light background.
  (when (and (not (display-graphic-p))
             (color-defined-p "brightwhite")
             (equal (face-foreground 'org-hide nil t) "white"))
    (set-face-foreground 'org-hide "brightwhite"))
  (add-hook 'org-mode-hook #'f-setup-buffer-org-main))

(defun f-setup-buffer-org-main ()
  (f-msg "INF" "`f-setup-buffer-org-main'")
  (f-setup-buffer-org-faces))

;; ** Face settings
;;    - See variable `face-remapping-alist' locally in an Org buffer.
;;    - History
;;      - 2015-09-29 More friendly with reverse video etc.
;;      - 2016-01-26 Change headings instead of misc Org faces
(defun f-setup-buffer-org-faces ()
  ;; - Deriving `org-block' from `shadow' seems wrong.
  ;; - The blue default foreground color of `org-table' is too annoying.
  ;; - The foreground colors of `org-level-3' and `org-level-4' are shared
  ;;   with other faces:
  ;;   - Blue1 ------ font-lock-function-name-face - outline-1 - org-level-1
  ;;   - sienna ----- font-lock-variable-name-face - outline-2 - org-level-2
  ;;   - Purple
  ;;       |- font-lock-keyword-face
  ;;       |    |- outline-3 - `org-level-3'
  ;;       |    '- org-special-keyword
  ;;       |------- org-date
  ;;       |------- org-footnote
  ;;       '------- org-sexp-date
  ;;   - Firebrick
  ;;       |- font-lock-comment-face
  ;;       |    |- outline-4 - `org-level-4'
  ;;       |    '- org-meta-line - org-block-begin-line - org-block-end-line
  ;;       |------- org-formula (table internals like "| <r> |")
  ;;       '------- org-upcoming-deadline
  ;;   - ForestGreen ----- font-lock-type-face ----- outline-5 - org-level-5
  ;;   - dark cyan ------- font-lock-constant-face - outline-6 - org-level-6
  ;;   - dark slate blue - font-lock-builtin-face -- outline-7 - org-level-7
  ;;   - VioletRed4 ------ font-lock-string-face --- outline-8 - org-level-8
  (pcase-dolist (`( ,replace    ,replacement)
                 '((org-block   default)
                   (org-table   default)
                   (org-level-3 outline-6) ; Replace with outline-5 or -6
                   (org-level-4 outline-7)
                   (org-level-5 org-level-1)
                   (org-level-6 org-level-2)
                   (org-level-7 org-level-3)
                   (org-level-8 org-level-4)))
    (face-remap-add-relative replace replacement))
  ;; The foreground color of `org-scheduled-previously' is not
  ;; distinguishable from `org-upcoming-deadline'. (face-remap-add-relative
  ;; 'org-scheduled-previously 'org-scheduled-today) in or outside of
  ;; `org-mode-hook' does not seem to work.
  (copy-face 'org-scheduled-today 'org-scheduled-previously)
  ;; The foreground colors of headlines with deadline in agenda view.
  ;; Argument FRACTION ("what fraction of the head-warning time has passed")
  ;; of function `org-agenda-deadline-face':
  ;; - (-inf..0.0): Before warning period, deadline not yet shown.
  ;; - 0.0:         Begin of warning period.
  ;; - (0.0..1.0):  During warning period.
  ;; - 1.0:         End of warning period, day of deadline.
  ;; - (1.0..inf):  After warning period, overdue.
  (setq org-agenda-deadline-faces
        ;; From larger to smaller.
        '(;; (1.0..inf)
          (1.00001 . org-warning)
          ;; [0.0..1.0]
          (0.0    . org-upcoming-deadline))))

(defun f-setup-feature-info-for-org ()
  (f-msg "INF" "`f-setup-feature-info-for-org'")
  ;; Not the friendliest but much simpler than fiddling with the INFOPATH
  ;; environment variable and its trailing colon.
  (require 'info)
  (info-initialize)
  (push (concat (file-name-as-directory v-d) "org-mode/doc")
        Info-directory-list))

(when (f-load-path-add v-d "org-mode/lisp")
  ;; Remove internal Org mode from `load-path'.
  (cl-delete-if (lambda (path) (string-match-p "/lisp/org$" path))
                load-path)
  (when (f-file-readable-p v-d "org-mode/doc")
    (add-hook 'Info-mode-hook #'f-setup-feature-info-for-org)))

;; This block is unconditional of `f-load-path-add' for the case of
;; fall-back to internal Org mode.
(setq-default org-use-extra-keys t) ; "You must set it before loading org."
(global-set-key (kbd "C-c e o") #'org-mode)
(f-feature 'org #'f-setup-feature-org-main)

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
(global-set-key (kbd "C-c e l") #'f-outline-minor-mode-toggle)

;; * Python mode (major mode)
;;   - History
;;     - 2016-09-22 Create
(defvar python-mode-map)

(defun f-setup-feature-python ()
  (f-msg "INF" "`f-setup-feature-python'")
  (define-key python-mode-map (kbd "C-c i") #'live-py-mode))

(f-feature 'python #'f-setup-feature-python)

;; * Minibuffer
;; Move (not copy) M-p/M-n to C-p/C-n in order to avoid the Meta key.
(pcase-dolist (`( ,key  ,func)
               '(("M-p" nil)
                 ("C-p" previous-history-element) ; Was unused
                 ("M-n" nil)
                 ("C-n" next-history-element))) ; Was unused
  (define-key minibuffer-local-map (kbd key) func))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
