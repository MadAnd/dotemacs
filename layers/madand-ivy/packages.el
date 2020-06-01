;;; packages.el --- madand-ivy layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst madand-ivy-packages
  '(counsel
    counsel-dash
    ivy
    swiper
    wgrep))

(defun madand-ivy/post-init-counsel ()
  (with-eval-after-load 'counsel
    (setq counsel-yank-pop-separator (concat "\n" (make-string 80 ?—) "\n"))
    (define-key counsel-find-file-map (kbd "C-u") 'counsel-up-directory)))

(defun madand-ivy/post-init-ivy ()
  (with-eval-after-load 'ivy
    (setq ivy-wrap t
          ivy-height 20)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-h") 'delete-backward-char)
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
    (evil-define-key 'normal ivy-occur-grep-mode-map
      (kbd "RET") 'ivy-occur-press-and-switch)
    (evil-define-key 'normal ivy-occur-mode-map
      (kbd "RET") 'ivy-occur-press-and-switch)))

(defun madand-ivy/post-init-swiper ()
  (with-eval-after-load 'swiper
    (define-key swiper-map (kbd "M-k") 'swiper-mc)))

(defun madand-ivy/post-init-wgrep ()
  (with-eval-after-load 'wgrep
    (setq wgrep-auto-save-buffer t)))

;;; packages.el ends here
