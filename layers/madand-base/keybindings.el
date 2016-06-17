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

;; We distinguish C-i and <tab> in GUI, but this distinction is not
;; neded in Hybrid state.
(evil-global-set-key 'hybrid [C-i] (kbd "<tab>"))
;; Serve a cup of Vim for the Hybrid state.
(evil-global-set-key 'hybrid (kbd "C-o") 'evil-execute-in-normal-state)
(evil-global-set-key  'hybrid (kbd "C-h") (kbd "DEL"))
(evil-global-set-key 'hybrid (kbd "C-S-h") help-map)
;; Swap ; and :
(evil-global-set-key 'normal (kbd ";") 'evil-ex)
(evil-global-set-key 'visual (kbd ";") 'evil-ex)
(evil-global-set-key 'normal (kbd ":") 'evil-repeat-find-char)
(evil-global-set-key 'visual (kbd ":") 'evil-repeat-find-char)
;; Make Q a buffer killer.
(evil-global-set-key 'normal (kbd "Q") 'spacemacs/kill-this-buffer)
(evil-global-set-key 'motion (kbd "Q") 'spacemacs/kill-this-buffer)
(evil-global-set-key 'evilified (kbd "Q") 'spacemacs/kill-this-buffer)
;; Unbind some Vim bindings, so Emacs' ones come into play.
(evil-global-set-key 'motion (kbd "C-e") nil)

;;; keybindings.el ends here
