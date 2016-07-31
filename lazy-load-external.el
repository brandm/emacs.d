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
  (auto-loads "cider" 'cider-jack-in)) ; A `defun'

;; * Clojure mode (major mode)
;;   - History
;;     - 2016-05-09 New
(when (load-path-add (concat loc-emacs-vc "/clojure-mode"))
  (auto-loads "clojure-mode"
              ;; They are all a `define-derived-mode'
              '(("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
                ("\\.cljc\\'" . clojurec-mode)
                ("\\.cljx\\'" . clojurex-mode)
                ("\\.cljs\\'" . clojurescript-mode)
                ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))))

;; * dash.el (library)
;;   - History
;;     - 2016-06-09 New
(load-path-add (concat loc-emacs-vc "/dash.el"))

;; * Extempore mode (major mode)
;;   - History
;;     - 2016-03-10 New
(when (load-path-add loc-emacs-pkg)
  (auto-loads "extempore"
              '("\\.xtm$" . extempore-mode)) ; A `define-derived-mode'
  (with-eval-after-load 'extempore-mode ; The `provide' in external
                                        ; extempore.el
    (setup-feature-extempore-mode)))

(defun setup-feature-extempore-mode ()
  (msg "INF" "`setup-feature-extempore-mode'")
  (setq-default extempore-share-directory "/f/x/git/extempore"))

;; * Matlab mode (major mode)
(when (load-path-add loc-emacs-pkg)
  (auto-loads "matlab"
              '(("\\.m$" . matlab-mode)     ; A `defun'
                (nil     . matlab-shell)))) ; A `defun'

;; * Paredit mode (minor mode)
;;   - History
;;     - 2016-02-18 New
(let ((function 'paredit-mode)) ; A `define-minor-mode'
  (when (load-path-add loc-emacs-pkg)
    (auto-loads "paredit" function)
    (global-set-key (kbd "C-c m p") function)
    (with-eval-after-load 'paredit ; The `provide' in external paredit.el
      (setup-feature-paredit))))

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
(load-path-add loc-emacs-pkg)

;; * scala-mode2
;;   - History
;;     - 2016-07-05 New
(when (load-path-add (concat loc-emacs-vc "/scala-mode2"))
  (auto-loads
   "scala-mode2"
   '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))) ; A `define-derived-mode'

;; * seq.el (library)
;;   - History
;;     - 2016-06-09 New
(load-path-add (concat loc-emacs-vc "/seq.el"))

;; * smartparens mode (minor mode)
;;   - History
;;     - 2016-06-09 New
(let ((function 'smartparens-mode)) ; A `define-minor-mode'
  (when (load-path-add (concat loc-emacs-vc "/smartparens"))
    (auto-loads "smartparens" function)
    (global-set-key (kbd "C-c m s") function)
    (with-eval-after-load 'smartparens ; The `provide' in external
                                       ; smartparens.el
      (setup-feature-smartparens))))

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
(load-path-add (concat loc-emacs-vc "/spinner.el"))

;; * File config :ARCHIVE:noexport:
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
