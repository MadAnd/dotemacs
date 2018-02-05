;;; funcs.el --- madand-lisp layer functions file for Spacemacs.
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

(defun madand/cider-eval-sexp-end-of-line (&optional prefix)
  "Evaluate the last sexp at the end of the current line."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (cider-eval-last-sexp prefix)))

(defun madand/slime-show-description (string package)
  "Overriding function for `slime-show-description'. It displays description
buffer with `help-mode', instead of default implementation's
`fundamental-mode'."
  (let ((bufname (slime-buffer-name :description)))
    (slime-with-popup-buffer (bufname :package package
                                      :connection t
                                      :select slime-description-autofocus
                                      :mode 'help-mode)
                             (princ string)
                             (goto-char (point-min)))))

;;; funcs.el ends here
