;;; keybindings.el --- madand-dvorak layer keybindings file for Spacemacs.
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

;; Adapt Window switching to Programmer Dvorak.
(madand-dvorak/loop-digit-keys
 (lambda (qwerty-key dvp-key)
   (spacemacs/set-leader-keys
     ;; Unset the QWERTY binding to prevent noise in the `which-key' output.
     qwerty-key nil
     dvp-key (if (string= qwerty-key "0")
                 #'treemacs-select-window
               (madand-dvorak//intern "winum-select-window-%s" qwerty-key)))))

;;; keybindings.el ends here
