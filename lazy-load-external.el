;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the external packages.

;; * CIDER mode (minor mode)
;;   - History
;;     - 2016-06-09 New
(when (f-load-path-add (concat v-loc-emacs-vc "cider"))
  (f-auto-loads "cider"
                'cider-connect 'cider-jack-in) ; They are all a `defun'
  (f-feature 'cider))

;; * Clojure mode (major mode)
;;   - History
;;     - 2016-05-09 New
(when (f-load-path-add (concat v-loc-emacs-vc "clojure-mode"))
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
;;     - 2016-06-09 New
(when (f-load-path-add (concat v-loc-emacs-vc "dash.el"))
  (f-feature 'dash))

;; * Extempore mode (major mode)
;;   - History
;;     - 2016-03-10 New
(defun f-setup-feature-extempore-mode ()
  (f-msg "INF" "`f-setup-feature-extempore-mode'")
  (setq-default extempore-share-directory "/f/x/git/extempore"))

(when (f-load-path-add v-loc-emacs-pkg)
  (f-auto-loads "extempore"
                '("\\.xtm$" . extempore-mode)) ; A `define-derived-mode'
  (f-feature '(extempore-mode "extempore.el")
             'f-setup-feature-extempore-mode))

;; * hy-mode (major mode)
;;   - History
;;     - 2016-10-10 New
(when (f-load-path-add (concat v-loc-emacs-vc "hy-mode"))
  (f-auto-loads "hy-mode"
                '("\\.hy\\'" . hy-mode)) ; A `define-derived-mode'
  (f-feature 'hy-mode))

;; * live-py-mode (minor mode)
;;   - History
;;     - 2016-09-22 New
(when (and (mapcar (lambda (p) (f-load-path-add (concat v-loc-emacs-vc p)))
                   '("/live-py-plugin/emacs-live-py-mode"
                     "/live-py-plugin/plugin/PySrc")))
  (f-auto-loads "live-py-mode"
                'live-py-mode) ; A `define-minor-mode'
  (f-feature 'live-py-mode))

;; * Matlab mode (major mode)
(when (f-load-path-add v-loc-emacs-pkg)
  (f-auto-loads "matlab"
                ;; They are all a `defun'.
                '("\\.m$" . matlab-mode) 'matlab-shell)
  (f-feature 'matlab))

;; * Paredit mode (minor mode)
;;   - History
;;     - 2016-02-18 New
(defun f-setup-feature-paredit ()
  (f-msg "INF" "`f-setup-feature-paredit'")
  (setq-default paredit-lighter " P") ; Was " Paredit" (leading space)
  ;; According to http://www.emacswiki.org/emacs-test/ParEdit
  (eldoc-add-command ; Updates the variable eldoc-message-commands
   'paredit-backward-delete
   'paredit-close-round))

(let ((func 'paredit-mode)) ; A `define-minor-mode'
  (when (f-load-path-add v-loc-emacs-pkg)
    (f-auto-loads "paredit" func)
    (global-set-key (kbd "C-c m p") func)
    (f-feature 'paredit 'f-setup-feature-paredit)))

;; * queue (library)
;;   - History
;;     - 2016-06-09 New
(when (f-load-path-add v-loc-emacs-pkg)
  (f-feature 'queue))

;; * scala-mode2
;;   - History
;;     - 2016-07-05 New
(when (f-load-path-add (concat v-loc-emacs-vc "scala-mode2"))
  (f-auto-loads "scala-mode2"
                ;; A `define-derived-mode'.
                '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
  (f-feature 'scala-mode2))

;; * seq.el (library)
;;   - History
;;     - 2016-06-09 New
(when (f-load-path-add (concat v-loc-emacs-vc "seq.el"))
  (f-feature 'seq))

;; * smartparens mode (minor mode)
;;   - History
;;     - 2016-06-09 New
(defvar smartparens-mode-map)
(declare-function sp-pair "smartparens")

(defun f-setup-feature-smartparens ()
  (f-msg "INF" "`f-setup-feature-smartparens'")
  ;; Remove from pairs.
  (mapc (lambda (x) (sp-pair x nil :actions :rem)) '("'" "`"))
  (setq-default sp-highlight-pair-overlay nil)
  (define-key smartparens-mode-map (kbd "C-c SPC") 'mark-sexp) ; No C-M-
  ;; C-{, C-(, C-} and C-) were unused.
  (define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp))

(let ((func 'smartparens-mode)) ; A `define-minor-mode'
  (when (f-load-path-add (concat v-loc-emacs-vc "smartparens"))
    (f-auto-loads "smartparens" func)
    (global-set-key (kbd "C-c m s") func)
    (f-feature 'smartparens 'f-setup-feature-smartparens)))

;; * spinner.el (library)
;;   - History
;;     - 2016-06-09 New
(when (f-load-path-add (concat v-loc-emacs-vc "spinner.el"))
  (f-feature 'spinner))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
