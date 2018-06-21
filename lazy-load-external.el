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
(when (f-load-path-add v-f)
  (f-auto-loads "adaptive-wrap"
                #'adaptive-wrap-prefix-mode) ; A `define-minor-mode'
  ;; When 6 it is to indent more than a possibly following hard wrapped
  ;; indent by 4.
  (setq-default adaptive-wrap-extra-indent 6)
  ;; For when a file is visited with this as a file local variable before
  ;; the feature has been loaded yet. Necessary even after v0.5.1 which
  ;; added `integerp' to the `defvar'.
  (put 'adaptive-wrap-extra-indent 'safe-local-variable #'integerp)
  (f-feature 'adaptive-wrap))

;; * CIDER mode (minor mode)
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-d "cider")
  (f-auto-loads "cider"
                #'cider-connect #'cider-jack-in) ; They are all a `defun'
  (f-feature 'cider))

;; * Clojure mode (major mode)
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
;;   - History
;;     - 2016-10-10 Create
(when (f-load-path-add v-d "hy-mode")
  (f-auto-loads "hy-mode"
                '("\\.hy\\'" . hy-mode)) ; A `define-derived-mode'
  (f-feature 'hy-mode))

;; * live-py-mode (minor mode)
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
;;   - History
;;     - 2017-08-27 Create.
(when (f-load-path-add v-d "origami.el")
  (f-auto-loads "origami"
                #'origami-mode) ; A `define-minor-mode'
  (f-feature 'origami))

;; * outorg (library)
;;   - History
;;     - 2017-08-27 Create.
(when (f-load-path-add v-d "outorg")
  (f-feature 'outorg))

;; * outshine (enhancement for Outline minor mode)
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
  (f-feature 'outshine #'f-setup-feature-outshine))

(global-set-key (kbd "C-c m m") #'f-outshine-toggle) ; Easiest to type

;; * Matlab mode (major mode)
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
  (f-feature 'paredit #'f-setup-feature-paredit))

(global-set-key (kbd "C-c m p") #'paredit-mode)

;; * queue (library)
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-f)
  (f-feature 'queue))

;; * scala-mode2 (major mode)
;;   - History
;;     - 2016-07-05 Create
(when (f-load-path-add v-d "scala-mode2")
  (f-auto-loads "scala-mode2"
                ;; A `define-derived-mode'.
                '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
  (f-feature 'scala-mode2))

;; * seq.el (library)
;;   - History
;;     - 2016-06-09 Create
(when (f-load-path-add v-d "seq.el")
  (f-feature 'seq))

;; * smartparens mode (minor mode)
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
  (pcase-dolist (`(,key ,func)
                 '(("C-c SPC" mark-sexp)               ; No C-M-
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
  (f-feature 'smartparens #'f-setup-feature-smartparens))

(global-set-key (kbd "C-c m s") #'smartparens-mode)

;; * spinner.el (library)
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
