;;; -*- lexical-binding: t -*-
;;; funcs.el --- madand-dvorak layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun madand-dvorak/loop-digit-keys (fun)
  "Loop trough pairs from `madand-dvorak-digit-keys-alist' calling FUN on
 each iteration.

The FUN is called like (fun qwerty-key dvp-key) where:
 * QWERTY-KEY is a string with a digit of QWERTY layout
 * DVP-KEY is a string with the corresponding character of Programmer Dvorak"
  (dolist (el madand-dvorak-digit-keys-alist)
    (funcall fun (car el) (cdr el))))

(defun madand-dvorak//intern (format-str &rest args)
  "`intern-soft' a result of passing FORMAT-STR with ARGS trough `format'."
  (intern-soft (apply 'format format-str args)))

(defun madand-dvorak//company-complete-number (n)
  "Generate a wrapped version of `company-complete-number'.
`company-complete-number' will be called with the value of N, if N is positive.
Otherwise it will be called with argument 10."
  (lambda ()
    (interactive)
    (company-complete-number (if (plusp n) n 10))))

;;; funcs.el ends here
