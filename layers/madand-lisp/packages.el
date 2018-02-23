;;; packages.el --- madand-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst madand-lisp-packages
  '(slime))

(defun madand-lisp/post-init-slime ()
  (with-eval-after-load 'slime
    (setq inferior-lisp-program "ros -Q run"
          slime-description-autofocus t)
    (advice-add 'slime-show-description
                :override #'madand/slime-show-description)))

;;; packages.el ends here
