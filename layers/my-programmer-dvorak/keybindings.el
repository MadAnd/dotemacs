;;; keybindings.el --- my-programmer-dvorak layer keybindings file for Spacemacs.
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

;; Adapt Window switching to Programmer Dvorak
(my-programmer-dvorak/loop-digit-keys
 (lambda (qwerty-key dvp-key)
   (spacemacs/set-leader-keys
     qwerty-key nil
     dvp-key (my-programmer-dvorak//intern "select-window-%s" qwerty-key))))

;;; keybindings.el ends here
