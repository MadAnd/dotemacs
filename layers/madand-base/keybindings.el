;;; keybindings.el --- madand-base layer keybindings for Spacemacs.
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

;; General keybindings.

;;; Code:

(spacemacs/set-leader-keys
  ";" #'evil-avy-goto-line
  "fF" #'find-file-at-point
  "id" #'madand/insert-date
  "o SPC" #'redraw-display
  "oS" #'scroll-all-mode
  "od" #'kill-buffer-and-window
  "ogd" #'madand/magit-tag-create-done
  "ogp" #'madand/magit-tag-create-prefixed
  "ogt" #'madand/magit-tag-create-tbreverted
  "on" #'spacemacs/scroll-transient-state/body
  "os" #'save-buffer
  "ot" #'google-translate-at-point
  "wO" #'delete-other-windows
  "qz" #'spacemacs/frame-killer)

(with-eval-after-load 'hybrid-mode
  ;; A cup of Vim in Hybrid State.
  (define-key evil-hybrid-state-map (kbd "C-o") 'evil-execute-in-normal-state)
  (define-key evil-hybrid-state-map (kbd "C-r") 'evil-paste-from-register)
  (define-key evil-hybrid-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-hybrid-state-map (kbd "C-S-h") help-map))

;; Make Q a buffer killer.
(define-key evil-normal-state-map (kbd "Q") 'spacemacs/kill-this-buffer)
(define-key evil-motion-state-map (kbd "Q") 'spacemacs/kill-this-buffer)
;; Unbind some Vim bindings, so that Emacs ones come into play.
(define-key evil-motion-state-map (kbd "C-e") nil)

(define-key evil-motion-state-map (kbd "zd") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "zu") 'evil-scroll-page-up)

;; Vim-ish Minibuffer
(define-key minibuffer-local-map (kbd "C-h") (kbd "DEL"))
(define-key minibuffer-local-map (kbd "C-S-h") help-map)

;;; keybindings.el ends here
