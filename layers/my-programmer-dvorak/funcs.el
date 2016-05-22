;;; funcs.el --- my-programmer-dvorak layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun my-programmer-dvorak/loop-digit-keys (fun)
  "Loop trough pairs from `my-programmer-dvorak-digit-keys-alist' calling FUN on each iteration.

The FUN is called like (fun qwerty-key dvp-key) where:
 * QWERTY-KEY is a string with a digit of QWERTY layout
 * DVP-KEY is a string with the corresponding character of Programmer Dvorak"
  (dolist (el my-programmer-dvorak-digit-keys-alist)
    (funcall fun (car el) (cdr el))))

(defun my-programmer-dvorak//intern (format-str &rest args)
  "`intern-soft' a result of passing FORMAT-STR with ARGS trough `format'."
  (intern-soft (apply 'format format-str args)))

;;; funcs.el ends here
