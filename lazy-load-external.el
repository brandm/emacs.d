;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2016 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the lazy load and setup of the external packages.

;; * CIDER mode (minor mode)
;;   - History
;;     - 2016-06-09 New
(when (load-path-add (concat loc-emacs-vc "/cider"))
  (auto-loads "cider"
              ;; They are all a `defun'.
              'cider-connect 'cider-jack-in)
  (feature 'cider))

;; * Clojure mode (major mode)
;;   - History
;;     - 2016-05-09 New
(when (load-path-add (concat loc-emacs-vc "/clojure-mode"))
  (auto-loads "clojure-mode"
              ;; They are all a `define-derived-mode'.
              '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
              '("\\.cljc\\'" . clojurec-mode)
              '("\\.cljx\\'" . clojurex-mode)
              '("\\.cljs\\'" . clojurescript-mode)
              '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  (feature 'clojure-mode))

;; * dash.el (library)
;;   - History
;;     - 2016-06-09 New
(when (load-path-add (concat loc-emacs-vc "/dash.el"))
  (feature 'dash))

;; * Extempore mode (major mode)
;;   - History
;;     - 2016-03-10 New
(when (load-path-add loc-emacs-pkg)
  (auto-loads "extempore"
              '("\\.xtm$" . extempore-mode)) ; A `define-derived-mode'
  (feature '(extempore-mode "extempore.el") 'setup-feature-extempore-mode))

(defun setup-feature-extempore-mode ()
  (msg "INF" "`setup-feature-extempore-mode'")
  (setq-default extempore-share-directory "/f/x/git/extempore"))

;; * hy-mode (major mode)
;;   - History
;;     - 2016-10-10 New
(when (load-path-add (concat loc-emacs-vc "/hy-mode"))
  (auto-loads
   "hy-mode"
   '("\\.hy\\'" . hy-mode)) ; A `define-derived-mode'
  (feature 'hy-mode))

;; * live-py-mode (minor mode)
;;   - History
;;     - 2016-09-22 New
(when (and (mapcar (lambda (p) (load-path-add (concat loc-emacs-vc p)))
                   '("/live-py-plugin/emacs-live-py-mode"
                     "/live-py-plugin/plugin/PySrc")))
  (auto-loads "live-py-mode"
              ;; A `define-minor-mode'
              'live-py-mode)
  (feature 'live-py-mode))

;; * Matlab mode (major mode)
(when (load-path-add loc-emacs-pkg)
  (auto-loads "matlab"
              ;; They are all a `defun'.
              '("\\.m$" . matlab-mode) 'matlab-shell)
  (feature 'matlab))

;; * Paredit mode (minor mode)
;;   - History
;;     - 2016-02-18 New
(let ((function 'paredit-mode)) ; A `define-minor-mode'
  (when (load-path-add loc-emacs-pkg)
    (auto-loads "paredit" function)
    (global-set-key (kbd "C-c m p") function)
    (feature 'paredit 'setup-feature-paredit)))

(defun setup-feature-paredit ()
  (msg "INF" "`setup-feature-paredit'")
  (setq-default paredit-lighter " P") ; Was " Paredit" (leading space)
  ;; According to http://www.emacswiki.org/emacs-test/ParEdit
  (eldoc-add-command ; Updates the variable eldoc-message-commands
   'paredit-backward-delete
   'paredit-close-round))

;; * queue (library)
;;   - History
;;     - 2016-06-09 New
(when (load-path-add loc-emacs-pkg)
  (feature 'queue))

;; * scala-mode2
;;   - History
;;     - 2016-07-05 New
(when (load-path-add (concat loc-emacs-vc "/scala-mode2"))
  (auto-loads
   "scala-mode2"
   '("\\.\\(scala\\|sbt\\)\\'" . scala-mode)) ; A `define-derived-mode'
  (feature 'scala-mode2))

;; * seq.el (library)
;;   - History
;;     - 2016-06-09 New
(when (load-path-add (concat loc-emacs-vc "/seq.el"))
  (feature 'seq))

;; * smartparens mode (minor mode)
;;   - History
;;     - 2016-06-09 New
(let ((function 'smartparens-mode)) ; A `define-minor-mode'
  (when (load-path-add (concat loc-emacs-vc "/smartparens"))
    (auto-loads "smartparens" function)
    (global-set-key (kbd "C-c m s") function)
    (feature 'smartparens 'setup-feature-smartparens)))

(defun setup-feature-smartparens ()
  (msg "INF" "`setup-feature-smartparens'")
  ;; Remove from pairs.
  (mapc (lambda (x) (sp-pair x nil :actions :rem)) '("'" "`"))
  (setq-default sp-highlight-pair-overlay nil)
  (define-key smartparens-mode-map (kbd "C-c SPC") 'mark-sexp) ; No C-M-
  ;; C-{, C-(, C-} and C-) were unused.
  (define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp))

;; * spinner.el (library)
;;   - History
;;     - 2016-06-09 New
(when (load-path-add (concat loc-emacs-vc "/spinner.el"))
  (feature 'spinner))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
