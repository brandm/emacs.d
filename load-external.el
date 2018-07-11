;; -*- lexical-binding: t -*-
;; * File comment
;;   - Copyright (C) 2000-2018 Michael Brand <michael.ch.brand at gmail.com>
;;   - Licensed under GPLv3, see http://www.gnu.org/licenses/gpl-3.0.html
;;   - URL: http://github.com/brandm/emacs.d
;;   - orgstruct-mode supported: On ";; *"-lines use TAB, S-TAB, C-TAB etc.
;;   - This file does the non-lazy load and setup of the external packages.

;; * God mode (minor mode)
;;   - http://github.com/chrisdone/god-mode
;;   - History
;;     - 2018-06-30 Create
(defun f-god-mode-refresh ()
  (if (and (boundp 'viper-current-state)
           (memq viper-current-state '(insert-state replace-state)))
      (god-local-mode-pause)
    (god-local-mode-resume)))

(when (f-load-path-add v-d "god-mode")
  (require 'god-mode)
  (f-god-mode-refresh)
  (add-hook 'window-configuration-change-hook #'f-god-mode-refresh))

;; * hydra
;;   - http://github.com/abo-abo/hydra
;;   - History
;;     - 2018-07-11 Create
(when (f-load-path-add v-d "hydra")
  (require 'hydra)
  ;; This mimics and obsoletes ibs.el used with `C-c v' (`ibs-select') and
  ;; `v' (`ibs-next-buffer'). ibs.el was used with the default `ibs-timeout
  ;; 4'.
  (defhydra hydra-switch-buffer (:timeout 4)
    ;; For example `previous-buffer' or `bs-cycle-previous'.
    ("v" previous-buffer))
  (global-set-key (kbd "C-c v") 'hydra-switch-buffer/previous-buffer))

;; * key-chord (minor mode)
;;   - http://www.emacswiki.org/emacs/KeyChord
;;   - Conflicts with other functionality that uses `input-method-function',
;;     for example Emacs Input Methods or ibs.el. "Key chord mode uses
;;     input-method-function. And so do internationalisation packages (mule,
;;     quail, etc). Do not expect them to work well together. The last one
;;     that gets the input-method-function rules."
;;   - History
;;     - 2018-06-28 Create
(defvar v-key-chord-control "en"
  "Key chord for `event-apply-control-modifier'.
The key chord is optimized for `vi-state' and the Colemak
keyboard layout (where `en' is QWERTY `jk').")

(defun f-key-chord-refresh ()
  ;; Refresh `input-method-function' as it seems to be reset to nil for
  ;; example when the current window changes the buffer with for example
  ;; `bs-show' or `ibs-select'.
  (setq input-method-function #'key-chord-input-method)
  (cond ((and (boundp 'viper-current-state)
              (memq viper-current-state '(insert-state replace-state)))
         (key-chord-define key-translation-map v-key-chord-control nil)
         (key-chord-define-global v-key-chord-control nil))
        (t
         (key-chord-define key-translation-map v-key-chord-control
                           #'event-apply-control-modifier)
         (key-chord-define-global v-key-chord-control
                                  #'event-apply-control-modifier))))

(when (f-load-path-add v-f)
  (require 'key-chord)
  (f-key-chord-refresh)
  (add-hook 'window-configuration-change-hook #'f-key-chord-refresh))

;; * File config
;;   Local Variables:
;;     coding: us-ascii-unix
;;     eval: (orgstruct-mode)
;;     fill-column: 76
;;     orgstruct-heading-prefix-regexp: " *;;;* "
;;   End:
