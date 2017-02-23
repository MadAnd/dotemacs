;;; packages.el --- my-programmer-dvorak layer packages file for Spacemacs.
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

(defconst my-programmer-dvorak-packages
  '(
    avy
    evil
    eyebrowse
    persp-mode
    ))

(defun my-programmer-dvorak/post-init-avy ()
  (setq avy-dispatch-alist my-programmer-dvorak-avy-dispatch-alist))

(defun my-programmer-dvorak/post-init-evil ()
  ;; Setting `evil-inhibit-esc' to t tells Evil not to translate C-[ into ESC,
  ;; so we can use it for Programmer Dvorak remapping of layout/workspace
  ;; binding C-2 to C-[.
  (setq evil-inhibit-esc t))

(defun my-programmer-dvorak/post-init-eyebrowse ()
  (my-programmer-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (my-programmer-dvorak//intern
                 "eyebrowse-switch-to-window-config-%s" qwerty-key)))
       (spacemacs/transient-state-register-add-bindings "workspaces"
         `(,(list dvp-key cmd :exit t)
           ,(list (concat "C-" dvp-key) cmd)))))))

(defun my-programmer-dvorak/post-init-persp-mode ()
  (my-programmer-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (my-programmer-dvorak//intern
                 "spacemacs/persp-switch-to-%s" qwerty-key)))
       (spacemacs/transient-state-register-add-bindings "layouts"
         `(,(list dvp-key cmd :exit t)
           ,(list (concat "C-" dvp-key) cmd)))))))

;;; packages.el ends here
