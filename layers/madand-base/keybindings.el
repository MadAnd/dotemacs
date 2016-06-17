;;; keybindings.el --- madand-base layer keybindings file for Spacemacs.
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

;; General keybindings.

;;; Code:

(spacemacs/set-leader-keys
  ";" #'evil-avy-goto-line
  "ij" (lambda (count)
         (interactive "p")
         (evil-save-state (evil-open-below count)))
  "ik" (lambda (count)
         (interactive "p")
         (evil-save-state (evil-open-above count)))
  "o SPC" #'redraw-display
  "o s" #'save-buffer
  "w O" #'delete-other-windows)

;;; keybindings.el ends here
