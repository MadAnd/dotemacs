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

(defvar madand-lisp-evilified-state-map (make-sparse-keymap)
  "Minimalist base keymap for evilification.")

(evil-define-key nil madand-lisp-evilified-state-map
  "h" 'evil-backward-char
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "l" 'evil-forward-char
  "zb" 'evil-scroll-line-to-bottom
  "zt" 'evil-scroll-line-to-top
  "zz" 'evil-scroll-line-to-center
  (kbd "C-z") 'evil-emacs-state)

;; (evilified-state-evilify-map sly-db-mode-map
;;   :mode sly-db-mode
;;   :eval-after-load sly
;;   :evilified-map madand-lisp-evilified-state-map
;;   :bindings
;;   "n"  'sly-db-down
;;   "p"  'sly-db-up
;;   ",n" 'sly-db-details-down
;;   ",p" 'sly-db-details-up
;;   "g"  'sly-db-beginning-of-backtrace
;;   "G"  'sly-db-end-of-backtrace
;;   "a"  'sly-db-abort
;;   "c"  'sly-db-continue
;;   "A"  'sly-db-break-with-system-debugger
;;   "B"  'sly-db-break-with-default-debugger
;;   "P"  'sly-db-print-condition
;;   "r"  'sly-db-invoke-restart-by-name
;;   "i"  'sly-db-inspect-condition
;;   ":"  'sly-interactive-eval
;;   "q"  'sly-db-quit)

;; (evilified-state-evilify-map sly-db-frame-mode-map
;;   :mode sly-db-frame-mode
;;   :eval-after-load sly
;;   :evilified-map madand-lisp-evilified-state-map
;;   :bindings
;;   )

(evilified-state-evilify-map sly-inspector-mode-map
  :mode sly-inspector-mode
  :evilified-map madand-lisp-evilified-state-map
  :eval-after-load sly
  :bindings
  ",b" 'sly-inspector-pop
  "gr" 'sly-inspector-reinspect
  "o" 'ace-link-slime-inspector
  "y" 'evil-yank)

;; (evilified-state-evilify-map sly-xref-mode-map
;;   :mode sly-xref-mode
;;   :eval-after-load sly
;;   :evilified-map madand-lisp-evilified-state-map
;;   :bindings
;;   "."  'sly-xref-goto
;;   "f"  'sly-xref-show
;;   "j"  'sly-xref-next-line
;;   "k"  'sly-xref-prev-line
;;   "n"  'sly-xref-next-line
;;   "o"  'ace-link-help
;;   "p"  'sly-xref-prev-line
;;   ",r" 'sly-recompile-xref
;;   ",R" 'sly-recompile-all-xrefs)

;;; keybindings.el ends here
