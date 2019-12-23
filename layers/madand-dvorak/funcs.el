;;; funcs.el --- madand-dvorak layer functions file for Spacemacs. -*- lexical-binding: t -*-
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

;;; Code:

(require 'cl-lib)

(cl-defmacro madand-dvorak|do-digit-keys ((qwerty-key dvp-key) &rest body)
  "Loop trough pairs in `madand-dvorak-digit-keys-alist'
evaluating BODY forms on each iteration.

QWERTY-KEY and DVP-KEY must be symbols that BODY forms can use to access
the current QWERTY/Dvorak key pair on each loop iteration."
  (declare (debug ((symbolp symbolp) body))
           (indent defun))
  (let ((key-pair (gensym)))
    `(dolist (,key-pair madand-dvorak-digit-keys-alist)
       (let ((,qwerty-key (car ,key-pair))
             (,dvp-key (cdr ,key-pair)))
         ,@body))))

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
