;;; packages.el --- my-programmer-dvorak layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <dev@madand.net>
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
    (default-helm-config :location built-in)
    eyebrowse
    persp-mode
    ))

(defun my-programmer-dvorak/pre-init-avy ()
  (spacemacs|use-package-add-hook avy
    :post-init
    (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?d))))

(defun my-programmer-dvorak/pre-init-ace-window ()
  (spacemacs|use-package-add-hook ace-window
    :post-init
    (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?d))))

(defun my-programmer-dvorak/pre-init-eyebrowse ()
  (spacemacs|use-package-add-hook eyebrowse
    :post-config
    (or (boundp 'spacemacs-workspaces-transient-state-add-bindings)
        (setq spacemacs-workspaces-transient-state-add-bindings '()))
    (my-programmer-dvorak/loop-digit-keys
     (lambda (qwerty-key dvp-key)
       (let ((cmd (my-programmer-dvorak//intern
                   "eyebrowse-switch-to-window-config-%s"
                   qwerty-key)))
         (push (list dvp-key cmd :exit t)
               spacemacs-workspaces-transient-state-add-bindings))))))

(defun my-programmer-dvorak/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (or (boundp 'spacemacs-layouts-transient-state-add-bindings)
        (setq spacemacs-layouts-transient-state-add-bindings'()))
    (my-programmer-dvorak/loop-digit-keys
     (lambda (qwerty-key dvp-key)
       (let ((cmd (my-programmer-dvorak//intern
                   "spacemacs/persp-switch-to-%s"
                   qwerty-key)))
         (push (list dvp-key cmd :exit t)
               spacemacs-layouts-transient-state-add-bindings))))))

;;; packages.el ends here
