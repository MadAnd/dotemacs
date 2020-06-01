;;; packages.el --- madand-dvorak layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst madand-dvorak-packages
  '(ace-window
    avy
    company
    evil
    eyebrowse
    persp-mode
    sly
    treemacs
    winum))

(defun madand-dvorak/pre-init-ace-window ()
  (setq aw-keys madand-dvorak-avy-keys))

(defun madand-dvorak/post-init-avy ()
  (setq avy-keys madand-dvorak-avy-keys
        avy-dispatch-alist madand-dvorak-avy-dispatch-alist))

(defun madand-dvorak/post-init-company ()
  (with-eval-after-load 'company
    (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
      (define-key company-active-map
        (kbd (format "M-%s" dvp-key))
        (madand-dvorak//company-complete-number
         (string-to-number qwerty-key))))))

(defun madand-dvorak/post-init-evil ()
  ;; Setting `evil-inhibit-esc' to t tells Evil not to translate C-[ into ESC,
  ;; so we can use it for Programmer Dvorak remapping of layout/workspace
  ;; binding C-2 to C-[.
  (setq evil-inhibit-esc t))

(defun madand-dvorak/post-init-eyebrowse ()
  (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
    (let ((cmd (madand-dvorak//intern
                "eyebrowse-switch-to-window-config-%s" qwerty-key)))
      (spacemacs/transient-state-register-add-bindings "workspaces"
        `(,(list dvp-key cmd :exit t)
          ,(list (concat "C-" dvp-key) cmd))))))

(defun madand-dvorak/post-init-persp-mode ()
  (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
    (let ((cmd (madand-dvorak//intern
                "spacemacs/persp-switch-to-%s" qwerty-key)))
      (spacemacs/transient-state-register-add-bindings "layouts"
        `(,(list dvp-key cmd :exit t)
          ,(list (concat "C-" dvp-key) cmd))))))

(defun madand-dvorak/post-init-sly ()
  (with-eval-after-load 'sly
    (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
      (define-key sly-db-mode-map
        (kbd dvp-key)
        (madand-dvorak//intern "sly-db-invoke-restart-%s" qwerty-key)))))

(defun madand-dvorak/pre-init-treemacs ()
  (spacemacs|use-package-add-hook treemacs
    :post-init
    (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
      (when (string= qwerty-key "0")
        (spacemacs/set-leader-keys
          qwerty-key nil
          dvp-key #'treemacs-select-window)))))

(defun madand-dvorak/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-init
    (madand-dvorak|do-digit-keys (qwerty-key dvp-key)
      ;; SPC 0 is reserved for neotree/treemacs window.
      (unless (string= qwerty-key "0")
        (spacemacs/set-leader-keys
          ;; Unset the QWERTY binding to prevent noise in the `which-key' output.
          qwerty-key nil
          dvp-key (madand-dvorak//intern "winum-select-window-%s" qwerty-key))))))

;;; packages.el ends here
