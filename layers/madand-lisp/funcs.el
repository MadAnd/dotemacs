;;; funcs.el --- madand-lisp layer functions file for Spacemacs.
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

(defun madand/elisp-flet-indent-function (indent-point state)
  "Handle `flet'-like lists, otherwise call `lisp-indent-function'."
  (cond
   ((ignore-errors
      (save-excursion
        (backward-up-list 3)
        (down-list)
        (when (memq (intern (thing-at-point 'symbol))
                    elisp-flet-style-macros)
          (forward-sexp 2)
          (>= (point) indent-point))))
    (lisp-indent-defform state indent-point))
   (t (lisp-indent-function indent-point state))))

;;; ----------------------------------------------------------------------------

(defun madand/sly-show-description (string package)
  "Overriding function for `slime-show-description'. It displays description
buffer with `help-mode'."
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (sly-buffer-name :description)))
    (sly-with-popup-buffer (bufname :package package
                                    :connection t
                                    :select sly-description-autofocus
                                    :mode 'help-mode)
      (sly-popup-buffer-mode)
      (princ string)
      (goto-char (point-min)))))

(defun madand/sly-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (sly-eval-last-expression)))



(defvar madand-lisp-src-dir "src/"
  "The path of the source files relative to the project root.")
(put 'madand-lisp-src-dir 'safe-local-variable #'stringp)

(defvar madand-lisp-test-dir "tests/"
  "The path of the test files relative to the (CL) project root.")
(put 'madand-lisp-test-dir 'safe-local-variable #'stringp)

(defun madand/project-type-common-lisp-p ()
  "Check whether *.asd files exist in the project root."
  (projectile-verify-file-wildcard "*.asd"))

(defun madand/projectile-find-implementation-or-test--around (f file-name)
  "Around advise for `projectile-find-implementation-or-test'

Call `madand/lisp-projectile-find-implementation-or-test', if the project type
is common-lisp. Otherwise, use the default implementation."
  (if (eq (projectile-project-type) 'common-lisp)
      (madand/lisp-find-implementation-or-test file-name)
    (funcall f file-name)))

(defun madand/lisp-find-implementation-or-test (file-name)
  "Find the implementation/test file for the given FILE-NAME.

This is the replacement of the `projectile-find-implementation-or-test' for
Common Lisp projects."
  (unless file-name (error "The current buffer is not visiting a file"))
  (let* ((project-root (projectile-project-root))
         (relative-path (replace-regexp-in-string
                         (regexp-quote project-root) "" file-name
                         t t))
         (is-test-file (string-prefix-p madand-lisp-test-dir relative-path)))
    (if is-test-file
        (replace-regexp-in-string (regexp-quote madand-lisp-test-dir)
                                  (regexp-quote madand-lisp-src-dir)
                                  file-name t t)
      (replace-regexp-in-string (regexp-quote madand-lisp-src-dir)
                                (regexp-quote madand-lisp-test-dir)
                                file-name t t))))

;;; ----------------------------------------------------------------------------

(defun spacemacs//sly-helm-source (&optional table)
  (or table (setq table sly-lisp-implementations))
  `((name . "Sly")
    (candidates . ,(mapcar #'car table))
    (action . (lambda (candidate)
                (car (helm-marked-candidates))))))

(defun spacemacs/helm-sly ()
  (interactive)
  (let ((command (helm :sources (spacemacs//sly-helm-source))))
    (and command (sly (intern command)))))

;;; funcs.el ends here
