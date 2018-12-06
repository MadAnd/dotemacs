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
  '((emacs-lisp :location built-in)
    lispy
    slime))

(defun madand-lisp/init-lispy ()
  (use-package lispy
    :init
    (progn
      (let ((mode-hooks '(emacs-lisp-mode-hook
                          lisp-mode-hook
                          slime-repl-mode-hook)))
        (spacemacs/add-to-hooks #'lispy-mode mode-hooks)
        ;; Disable Smartparens altogether since Lispy already does the same.
        (when (configuration-layer/package-usedp 'smartparens)
          (spacemacs/add-to-hooks #'turn-off-smartparens-mode mode-hooks))))
    :config
    (progn
      )))

(defun madand-lisp/post-init-slime ()
  (spacemacs|use-package-add-hook slime
    :post-config
    (progn
      (setq ;; inferior-lisp-program "qlot exec ros -S . run"
            inferior-lisp-program "ros -Q run"
            slime-description-autofocus t)
      (advice-add 'slime-show-description
                  :override #'madand/slime-show-description)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "si" #'slime-qlot-exec))))

;;; packages.el ends here
