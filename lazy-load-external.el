;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the external packages.

;; * adaptive-wrap (minor mode)
;;   - History
;;     - 2017-06-11 Create
(defun f-setup-feature-adaptive-wrap ()
  (f-msg "INF" "`f-setup-feature-adaptive-wrap'")
  ;; When 6 it is to indent more than a possibly following hard wrapped
  ;; indent by 4.
  (setq-default adaptive-wrap-extra-indent 6))

(when (f-load-path-add v-f)
  (f-auto-loads "adaptive-wrap"
                #'adaptive-wrap-prefix-mode) ; A `define-minor-mode'
  ;; For when a file is visited with this as a file local variable possibly
  ;; before the feature has been loaded yet. Necessary even after v0.5.1
  ;; which added `integerp' to the `defvar'.
  (put 'adaptive-wrap-extra-indent 'safe-local-variable #'integerp)
  (f-feature 'adaptive-wrap #'f-setup-feature-adaptive-wrap))

;; * Bookmarks
;;   - http://github.com/joodland/bm
;;   - History
;;     - 2001-11-12 Create
;;     - 2018-06-22 Moved to here and refactored
(defun f-setup-feature-bm ()
  (f-msg "INF" "`f-setup-feature-bm'")
  ;; White is not considered to be useful and on most terminals it is a dark
  ;; white that is hard to distinguish from the background.
  (when (equal (face-foreground 'bm-face nil t) "White")
    (set-face-foreground 'bm-face nil)))

(when (f-load-path-add v-d "bm")
  (f-auto-loads "bm"
                ;; They are all a `defun'.
                #'bm-next #'bm-previous #'bm-toggle)
  (pcase-dolist (`(,key ,func) '(("C-c n" bm-next)
                                 ("C-c p" bm-previous)
                                 ("C-c r" bm-toggle)))
    (global-set-key (kbd key) func))
  (f-feature 'bm #'f-setup-feature-bm))

;; * CIDER mode (minor mode)
;;   - http://github.com/clojure-emacs/cider
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-d "cider")
  (f-auto-loads "cider"
                #'cider-connect #'cider-jack-in) ; They are all a `defun'
  (f-feature 'cider))

;; * Clojure mode (major mode)
;;   - http://github.com/clojure-emacs/clojure-mode
;;   - History
;;     - 2016-05-09 Create
(when (f-load-path-add v-d "clojure-mode")
  (f-auto-loads "clojure-mode"
                ;; They are all a `define-derived-mode'.
                '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
                '("\\.cljc\\'" . clojurec-mode)
                '("\\.cljx\\'" . clojurex-mode)
                '("\\.cljs\\'" . clojurescript-mode)
                '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  (f-feature 'clojure-mode))

;; * dash.el (library)
;;   - http://github.com/magnars/dash.el
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-d "dash.el")
  (f-feature 'dash))

;; * Extempore mode (major mode)
;;   - History
;;     - 2016-03-10 Create
(defun f-setup-feature-extempore-mode ()
  (f-msg "INF" "`f-setup-feature-extempore-mode'")
  (setq-default extempore-share-directory "/f/x/git/extempore"))

(when (f-load-path-add v-f)
  (f-auto-loads "extempore"
                '("\\.xtm$" . extempore-mode)) ; A `define-derived-mode'
  (f-feature '(extempore-mode "extempore.el")
             #'f-setup-feature-extempore-mode))

;; * hy-mode (major mode)
;;   - http://github.com/hylang/hy-mode
;;   - History
;;     - 2016-10-10 Create
(when (f-load-path-add v-d "hy-mode")
  (f-auto-loads "hy-mode"
                '("\\.hy\\'" . hy-mode)) ; A `define-derived-mode'
  (f-feature 'hy-mode))

;; * live-py-mode (minor mode)
;;   - http://github.com/donkirkby/live-py-plugin
;;   - Keep in sync with `f-setup-feature-python'.
;;   - History
;;     - 2016-09-22 Create
(defun f-setup-feature-live-py-mode ()
  (f-msg "INF" "`f-setup-feature-live-py-mode'")
  (setq-default live-py-lighter-delaying " Live-D"
                live-py-lighter-tracing  " Live-T"
                live-py-lighter-ready    " Live-t"
                live-py-lighter-fail     " Live-F"))

(when (cl-every (lambda (subdir) (f-load-path-add v-d subdir))
                '("live-py-plugin/emacs-live-py-mode"
                  "live-py-plugin/plugin/PySrc"))
  (f-auto-loads "live-py-mode"
                'live-py-mode) ; A `define-minor-mode'
  (f-feature 'live-py-mode #'f-setup-feature-live-py-mode))

;; * origami.el (minor mode)
;;   - http://github.com/gregsexton/origami.el
;;   - History
;;     - 2017-08-27 Create.
(defun f-setup-feature-origami ()
  (f-msg "INF" "`f-setup-feature-origami'")
  (define-key
    origami-mode-map (kbd "C-c TAB") 'origami-recursively-toggle-node))

(when (f-load-path-add v-d "origami.el")
  (f-auto-loads "origami"
                #'origami-mode) ; A `define-minor-mode'
  (f-feature 'origami #'f-setup-feature-origami))

;; * outorg (library)
;;   - http://github.com/alphapapa/outorg
;;   - History
;;     - 2017-08-27 Create.
(when (f-load-path-add v-d "outorg")
  (f-feature 'outorg))

;; * outshine (enhancement for Outline minor mode)
;;   - http://github.com/alphapapa/outshine
;;   - History
;;     - 2017-08-27 Create.
(defun f-setup-feature-outshine ()
  (f-msg "INF" "`f-setup-feature-outshine'")
  (setq-default
   ;; Like Org config. In Viper mode available only in Emacs state (E).
   outshine-use-speed-commands t
   outshine-fontify (lambda () (not (derived-mode-p 'prog-mode))))
  ;; `outshine-hook-function' is not added earlier to
  ;; `outline-minor-mode-hook' to support plain `outline-minor-mode' without
  ;; outshine before outshine has been used the first time.
  ;; `outline-minor-mode' can not be used without outshine after outshine
  ;; has been used the first time also when `outshine-hook-function' is not
  ;; added to `outline-minor-mode-hook' but `outshine-hook-function' is
  ;; called after `outline-minor-mode' to start outshine. See also the
  ;; `user-error' in `f-outline-minor-mode-toggle'.
  (dolist (func '(outshine-hook-function f-setup-buffer-outshine))
    (add-hook 'outline-minor-mode-hook func t)))

(defun f-setup-buffer-outshine ()
  ;; `outline-minor-mode-hook' is also run when `outline-minor-mode' is
  ;; _leaving_.
  (f-msg "INF" (format "`f-setup-buffer-outshine'%s"
                       (if outline-minor-mode
                           ""
                         " (Outline minor mode is leaving)")))
  (when outline-minor-mode ; `outline-minor-mode' is entering
    (setq outshine-org-style-global-cycling-at-bob-p
          (not (outline-on-heading-p)))))

(defun f-outshine-toggle ()
  (interactive)
  (if outline-minor-mode
      (outline-minor-mode 0) ; Turn off
    (require 'outshine)
    (outline-minor-mode))) ; Turn on

(when (f-load-path-add v-d "outshine")
  (f-auto-loads "outshine"
                #'outshine-hook-function) ; A `defun'
  (global-set-key (kbd "C-c e e") #'f-outshine-toggle)
  (f-feature 'outshine #'f-setup-feature-outshine))

;; * Matlab mode (major mode)
;;   - History
;;     - 2002-03-11 Create
(when (f-load-path-add v-f)
  (f-auto-loads "matlab"
                ;; They are all a `defun'.
                '("\\.m$" . matlab-mode) #'matlab-shell)
  (f-feature 'matlab))

;; * Paredit mode (minor mode)
;;   - History
;;     - 2016-02-18 Create
(defun f-setup-feature-paredit ()
  (f-msg "INF" "`f-setup-feature-paredit'")
  (setq-default paredit-lighter " P") ; Was " Paredit" (leading space)
  ;; According to http://www.emacswiki.org/emacs-test/ParEdit
  (eldoc-add-command ; Updates the variable eldoc-message-commands
   'paredit-backward-delete
   'paredit-close-round))

(when (f-load-path-add v-f)
  (f-auto-loads "paredit"
                #'paredit-mode) ; A `define-minor-mode'
  (global-set-key (kbd "C-c e p") #'paredit-mode)
  (f-feature 'paredit #'f-setup-feature-paredit))

;; * queue (library)
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-f)
  (f-feature 'queue))

;; * s.el (library)
;;   - http://github.com/magnars/s.el
;;   - History
;;     - 2018-06-24 Create
(when (f-load-path-add v-d "s.el")
  (f-feature 's))

;; * smartparens mode (minor mode)
;;   - http://github.com/Fuco1/smartparens
;;   - History
;;     - 2016-06-09 Create
(defvar smartparens-mode-map)
(declare-function sp-pair "smartparens")

(defun f-setup-feature-smartparens ()
  (f-msg "INF" "`f-setup-feature-smartparens'")
  ;; Remove from pairs.
  (dolist (open '("'" "`"))
    (sp-pair open nil :actions :rem))
  (setq-default sp-highlight-pair-overlay nil)
  (pcase-dolist (`( ,key      ,func)
                 '(("C-c SPC" mark-sexp)               ; No `C-M-'
                   ("C-{"     sp-backward-barf-sexp)   ; Was unused
                   ("C-("     sp-backward-slurp-sexp)  ; Was unused
                   ("C-}"     sp-forward-barf-sexp)    ; Was unused
                   ("C-)"     sp-forward-slurp-sexp))) ; Was unused
    (define-key smartparens-mode-map (kbd key) func)))

(when (f-load-path-add v-d "smartparens")
  (f-auto-loads "smartparens"
                #'smartparens-mode) ; A `define-minor-mode'
  (dolist (hook '(;; Lisp dialects
                  clojure-mode-hook
                  eval-expression-minibuffer-setup-hook
                  extempore-mode-hook
                  hy-mode-hook
                  ;; Keep this rest in sync with adding `eldoc-mode' to the
                  ;; same hooks.
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook))
    (add-hook hook #'smartparens-mode))
  (global-set-key (kbd "C-c e s") #'smartparens-mode)
  (f-feature 'smartparens #'f-setup-feature-smartparens))

;; * spinner.el (library)
;;   - http://github.com/Malabarba/spinner.el
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-d "spinner.el")
  (f-feature 'spinner))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
