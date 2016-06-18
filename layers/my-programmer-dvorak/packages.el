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
    ace-window
    evil
    eyebrowse
    persp-mode
    ))

(defun my-programmer-dvorak/post-init-avy ()
  (setq avy-keys my-programmer-dvorak-jump-keys
        avy-dispatch-alist my-programmer-dvorak-avy-dispatch-alist))

(defun my-programmer-dvorak/post-init-ace-window ()
  (setq aw-keys my-programmer-dvorak-jump-keys))

(defun my-programmer-dvorak/post-init-evil ()
  ;; Setting `evil-intercept-esc' to t tells Evil not to translate C-[ into ESC
  ;; in GUI, so we can use it for Programmer Dvorak remapping of layout/workspace
  ;; binding C-2 to C-[.
  (setq evil-intercept-esc t))

(defun my-programmer-dvorak/post-init-eyebrowse ()
  (or (boundp 'spacemacs-workspaces-transient-state-add-bindings)
      (setq spacemacs-workspaces-transient-state-add-bindings '()))
  (my-programmer-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (my-programmer-dvorak//intern
                 "eyebrowse-switch-to-window-config-%s" qwerty-key)))
       (push (list dvp-key cmd :exit t)
             spacemacs-workspaces-transient-state-add-bindings)
       (push (list (concat "C-" dvp-key) cmd)
             spacemacs-workspaces-transient-state-add-bindings) ))))

(defun my-programmer-dvorak/post-init-persp-mode ()
  (or (boundp 'spacemacs-layouts-transient-state-add-bindings)
      (setq spacemacs-layouts-transient-state-add-bindings'()))
  (my-programmer-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (my-programmer-dvorak//intern
                 "spacemacs/persp-switch-to-%s" qwerty-key)))
       (push (list dvp-key cmd :exit t)
             spacemacs-layouts-transient-state-add-bindings)
       ;; We can properly handle things like C-[ only in GUI.
       (when (display-graphic-p)
         (push (list (concat "C-" dvp-key) cmd)
               spacemacs-layouts-transient-state-add-bindings))))))

;;; packages.el ends here
