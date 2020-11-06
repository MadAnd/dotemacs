;;; keybindings.el --- madand-lisp layer keybindings for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; General keybindings.

;;; Code:

(with-eval-after-load 'sly
  (evil-define-key 'normal sly-mode-map
    "gb" 'slime-pop-find-definition-stack)
  (evil-define-key 'normal sly-popup-buffer-mode-map
    "Q" #'kill-buffer-and-window)
  ;; Lisp mode leader bindings
  (dolist (x '(("m=" . "format")
               ("mh" . "help/doc/xref")
               ("mc" . "compile")
               ("me" . "eval")
               ("mE" . "errors/notes")
               ("mg" . "navigation")
               ("mi" . "import/export")
               ("mm" . "macro")
               ("ms" . "repl")
               ("mS" . "stickers")
               ("mt" . "trace")))
    (spacemacs/declare-prefix-for-mode 'sly-mode (car x) (cdr x)))
  (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
    "'"  'sly-mrepl
    "." 'spacemacs/common-lisp-navigation-transient-state/body
    ;; Code Formatting
    "==" 'indent-sexp
    "=b" 'madand-lisp/indent-buffer
    "=f" 'madand-lisp/indent-defun
    ;; Help
    "ha" 'sly-apropos
    "hA" 'sly-apropos-all
    "hb" 'sly-who-binds
    "hd" 'sly-disassemble-symbol
    "hg" 'common-lisp-hyperspec-glossary-term
    "hh" 'sly-describe-symbol
    "hi" 'sly-inspect
    "hl" 'sly-hyperspec-lookup
    "hm" 'sly-who-macroexpands
    "hp" 'sly-apropos-package
    "hr" 'sly-who-references
    "hs" 'sly-who-specializes
    "hS" 'sly-who-sets
    "h<" 'sly-who-calls
    "h>" 'sly-calls-who
    "h#" 'common-lisp-hyperspec-lookup-reader-macro
    "h~" 'common-lisp-hyperspec-format
    ;; Compilation
    "cc" 'sly-compile-file
    "cC" 'sly-compile-and-load-file
    "cf" 'sly-compile-defun
    "cl" 'sly-load-file
    "cr" 'sly-compile-region
    ;; Evaluation
    "eb" 'sly-eval-buffer
    "ec" 'madand/sly-eval-current-form-sp
    "ee" 'sly-eval-last-expression
    "ef" 'sly-eval-defun
    "el" 'madand/sly-eval-sexp-end-of-line
    "er" 'sly-eval-region
    "ep" 'sly-pprint-eval-last-expression
    "eP" 'sly-eval-print-last-expression
    "eu" 'sly-unintern-symbol
    "eU" 'sly-undefine-function
    "ev" 'sly-edit-value
    ;; Errors/notes
    "nn" 'sly-next-note
    "np" 'sly-previous-note
    "nr" 'sly-remove-notes
    ;; Navigation
    "gb" 'sly-pop-find-definition-stack
    "gi" 'madand-lisp/sly-goto-imports
    "gu" 'sly-edit-uses
    ;; Import/export
    "ii" 'sly-import-symbol-at-point
    "ie" 'sly-export-symbol-at-point
    "ic" 'sly-export-class
    ;; Macroexpand
    "m." 'spacemacs/macrostep-transient-state/body
    "me" 'sly-expand-1
    "mE" 'sly-macroexpand-all
    ;; REPL
    "sc" 'sly-mrepl-clear-repl
    "si" 'sly
    "sI" 'sly-interrupt
    "sl" 'sly-list-connections
    "sN" 'sly-mrepl-new
    "sn" 'sly-next-connection
    "sp" 'sly-prev-connection
    "sq" 'sly-quit-lisp
    "sQ" 'madand-lisp/sly-quit-current-lisp
    "sr" 'sly-restart-inferior-lisp
    "ss" 'sly-mrepl-sync
    "sw" 'madand/sly-connect-to-stumpwm
    ;; Stickers
    "Sb" 'sly-stickers-toggle-break-on-stickers
    "Sc" 'sly-stickers-clear-defun-stickers
    "SC" 'sly-stickers-clear-buffer-stickers
    "Sf" 'sly-stickers-fetch
    "Sr" 'sly-stickers-replay
    "Ss" 'sly-stickers-dwim
    ;; trace
    "td" 'sly-trace-dialog
    "tt" 'sly-trace-dialog-toggle-trace
    "tT" 'sly-toggle-fancy-trace
    "tu" 'sly-untrace-all)
  ;; `sly-mrepl-mode' bindings are a subset of those of `lisp-mode'
  (dolist (x '(
               ("mh" . "help/doc/xref")
               ("mg" . "navigation")
               ("me" . "eval")
               ("ms" . "repl")))
    (spacemacs/declare-prefix-for-mode 'sly-mrepl-mode (car x) (cdr x)))
  (spacemacs/set-leader-keys-for-major-mode 'sly-mrepl-mode
    ;; Help
    "ha" 'sly-apropos
    "hA" 'sly-apropos-all
    "hb" 'sly-who-binds
    "hd" 'sly-disassemble-symbol
    "hg" 'common-lisp-hyperspec-glossary-term
    "hh" 'sly-describe-symbol
    "hi" 'sly-inspect
    "hl" 'sly-hyperspec-lookup
    "hm" 'sly-who-macroexpands
    "hp" 'sly-apropos-package
    "hr" 'sly-who-references
    "hs" 'sly-who-specializes
    "hS" 'sly-who-sets
    "h<" 'sly-who-calls
    "h>" 'sly-calls-who
    "h#" 'common-lisp-hyperspec-lookup-reader-macro
    "h~" 'common-lisp-hyperspec-format
    ;; Eval
    "ec" 'madand/sly-eval-current-form-sp
    "eu" 'sly-unintern-symbol
    "eU" 'sly-undefine-function
    "ev" 'sly-edit-value
    ;; Navigation
    "gd" 'sly-edit-definition-other-window
    "gu" 'sly-edit-uses
    ;; REPL
    "sc" 'sly-mrepl-clear-repl
    "si" 'sly-interrupt
    "sl" 'sly-list-connections
    "sN" 'sly-mrepl-new
    "sn" 'sly-next-connection
    "sp" 'sly-prev-connection
    "sq" 'sly-quit-lisp
    "sQ" 'madand-lisp/sly-quit-current-lisp
    "sr" 'sly-restart-inferior-lisp
    "sw" 'madand/sly-connect-to-stumpwm))

;;; ----------------------------------------------------------------------------
;;; Base evilification map
;;; ----------------------------------------------------------------------------

(defvar madand-lisp-evilified-state-map (make-sparse-keymap)
  "Minimalist base keymap for evilification.")

(evil-define-key nil madand-lisp-evilified-state-map
  "h" 'evil-backward-char
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "l" 'evil-forward-char
  "y" 'evil-yank
  "Y" 'evil-yank-line
  "zb" 'evil-scroll-line-to-bottom
  "zt" 'evil-scroll-line-to-top
  "zz" 'evil-scroll-line-to-center
  (kbd "C-z") 'evil-emacs-state)

;;; ----------------------------------------------------------------------------
;;; Debugger
;;; ----------------------------------------------------------------------------

(evilified-state-evilify-map sly-db-mode-map
  :mode sly-db-mode
  :eval-after-load sly
  :evilified-map madand-lisp-evilified-state-map
  :bindings
  "j"  'sly-db-down
  "k"  'sly-db-up
  "gj" 'sly-db-details-down
  "gk" 'sly-db-details-up
  "gg"  'sly-db-beginning-of-backtrace
  "G"  'sly-db-end-of-backtrace
  "a"  'sly-db-abort
  "c"  'sly-db-continue
  "A"  'sly-db-break-with-system-debugger
  "B"  'sly-db-break-with-default-debugger
  "p"  'sly-db-print-condition
  "r"  'sly-db-invoke-restart-by-name
  "i"  'sly-db-inspect-condition
  "ge"  'sly-interactive-eval
  "gq"  'sly-db-quit
  ;; Additional non-sly goodies
  "o" 'ace-link-help)

(with-eval-after-load 'sly
  (evil-define-key nil sly-db-frame-map
    (kbd "t")   'sly-db-toggle-details
    (kbd "v")   'sly-db-show-frame-source
    (kbd "gd")   'sly-db-disassemble
    (kbd "e")   'sly-db-eval-in-frame
    (kbd "d")   'sly-db-pprint-eval-in-frame
    (kbd "i")   'sly-db-inspect-in-frame
    (kbd "r")   'sly-db-restart-frame
    (kbd "gr")   'sly-db-return-from-frame
    (kbd "RET") 'sly-db-toggle-details
    "s"    'sly-db-step
    "x"    'sly-db-next
    "go"    'sly-db-out
    "b"    'sly-db-break-on-return
    "\C-c\C-c" 'sly-db-recompile-frame-source))

;;; ----------------------------------------------------------------------------
;;; Inspector
;;; ----------------------------------------------------------------------------

(evilified-state-evilify-map sly-inspector-mode-map
  :mode sly-inspector-mode
  :evilified-map madand-lisp-evilified-state-map
  :eval-after-load sly
  :bindings
  "G"  'sly-inspector-fetch-all
  "gb" 'sly-inspector-pop
  "gn" 'sly-inspector-next
  "gr" 'sly-inspector-reinspect
  "gd" 'sly-inspector-describe-inspectee
  "ge" 'sly-inspector-eval
  "gh" 'sly-inspector-history
  ;; Additional non-sly goodies
  "o" 'ace-link-help)

;;; ----------------------------------------------------------------------------
;;; xref
;;; ----------------------------------------------------------------------------

(evilified-state-evilify-map sly-xref-mode-map
  :mode sly-xref-mode
  :eval-after-load sly
  :evilified-map madand-lisp-evilified-state-map
  :bindings
  "."  'sly-xref-goto
  "f"  'sly-xref-show
  "gj" 'sly-xref-next-line
  "gk" 'sly-xref-prev-line
  "n"  'sly-xref-next-line
  "p"  'sly-xref-prev-line
  "gr" 'sly-recompile-xref
  "gR" 'sly-recompile-all-xrefs
  ;; Additional non-sly goodies
  "o" 'ace-link-help)

;;; ----------------------------------------------------------------------------
;;; Connection List
;;; ----------------------------------------------------------------------------

(evilified-state-evilify-map sly-connection-list-mode-map
  :mode sly-connection-list-mode
  :eval-after-load sly
  :evilified-map madand-lisp-evilified-state-map
  :bindings
  "f"  'sly-connection-list-default-action
  "gd" 'sly-connection-list-make-default
  "gr" 'sly-update-connection-list
  "dd" 'sly-quit-connection-at-point
  "rr"  'sly-restart-connection-at-point
  "q"  'quit-window
  (kbd "RET") 'sly-connection-list-default-action
  (kbd "C-m") 'sly-connection-list-default-action
  ;; Additional non-sly goodies
  "o" 'ace-link-help)
