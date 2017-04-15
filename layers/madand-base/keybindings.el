;;; keybindings.el --- madand-base layer keybindings for Spacemacs.
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
  "id" #'madand/insert-date
  "o SPC" #'redraw-display
  "os" #'save-buffer
  "oS" #'scroll-all-mode
  "ogp" #'madand/magit-tag-create-prefixed
  "ogt" #'madand/magit-tag-create-tbreverted
  "ogd" #'madand/magit-tag-create-done
  "wO" #'delete-other-windows)

;; We distinguish C-i and <tab> in GUI, but this distinction is not
;; desired in Hybrid state.
(define-key evil-hybrid-state-map [C-i] (kbd "<tab>"))
;; A cup of Vim for the Hybrid state.
(define-key evil-hybrid-state-map (kbd "C-o") 'evil-execute-in-normal-state)
(define-key evil-hybrid-state-map (kbd "C-r") 'evil-paste-from-register)
(define-key evil-hybrid-state-map (kbd "C-h") (kbd "DEL"))
(define-key evil-hybrid-state-map (kbd "C-S-h") help-map)
;; Make Q a buffer killer.
(define-key evil-normal-state-map (kbd "Q") 'spacemacs/kill-this-buffer)
(define-key evil-motion-state-map (kbd "Q") 'spacemacs/kill-this-buffer)
;; Unbind some Vim bindings, so that Emacs ones come into play.
(define-key evil-motion-state-map (kbd "C-e") nil)

;;; keybindings.el ends here
