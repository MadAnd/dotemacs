;;; packages.el --- madand-dvorak layer packages file for Spacemacs.
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

(defconst madand-dvorak-packages
  '(
    avy
    company
    evil
    eyebrowse
    persp-mode
    ))

(defun madand-dvorak/post-init-avy ()
  (setq avy-dispatch-alist madand-dvorak-avy-dispatch-alist))

(defun madand-dvorak/post-init-company ()
  (with-eval-after-load 'company
    (madand-dvorak/loop-digit-keys
     (lambda (qwerty-key dvp-key)
       (define-key company-active-map
         (kbd (format "M-%s" dvp-key))
         (madand-dvorak//company-complete-number (string-to-int qwerty-key)))))))

(defun madand-dvorak/post-init-evil ()
  ;; Setting `evil-inhibit-esc' to t tells Evil not to translate C-[ into ESC,
  ;; so we can use it for Programmer Dvorak remapping of layout/workspace
  ;; binding C-2 to C-[.
  (setq evil-inhibit-esc t))

(defun madand-dvorak/post-init-eyebrowse ()
  (madand-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (madand-dvorak//intern
                 "eyebrowse-switch-to-window-config-%s" qwerty-key)))
       (spacemacs/transient-state-register-add-bindings "workspaces"
         `(,(list dvp-key cmd :exit t)
           ,(list (concat "C-" dvp-key) cmd)))))))

(defun madand-dvorak/post-init-persp-mode ()
  (madand-dvorak/loop-digit-keys
   (lambda (qwerty-key dvp-key)
     (let ((cmd (madand-dvorak//intern
                 "spacemacs/persp-switch-to-%s" qwerty-key)))
       (spacemacs/transient-state-register-add-bindings "layouts"
         `(,(list dvp-key cmd :exit t)
           ,(list (concat "C-" dvp-key) cmd)))))))

;;; packages.el ends here