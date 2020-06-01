;;; packages.el --- madand-typescript Layer packages File for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; This is stripped-down and personalized version of the official typescript
;; layer. Mainly all conditional code for different backends was deleted since
;; LSP and PrettierJS is assumed in my setup.

(setq madand-typescript-packages
      '(eldoc
        emmet-mode
        flycheck
        import-js
        lsp-mode
        prettier-js
        smartparens
        typescript-mode
        web-mode
        yasnippet))

(defun madand-typescript/post-init-eldoc ()
  (spacemacs/add-to-hooks #'eldoc-mode '(typescript-mode-hook
                                         typescript-tsx-mode-hook)
                          'append))

(defun madand-typescript/post-init-emmet-mode ()
  (add-hook 'typescript-tsx-mode-hook #'madand-typescript//setup-emmet-mode))

(defun madand-typescript/pre-init-import-js ()
  (cl-loop for spec in '((typescript-mode . typescript-mode-hook)
                         (typescript-tsx-mode . typescript-tsx-mode-hook))

           do (add-to-list 'spacemacs--import-js-modes spec)))

(defun madand-typescript/post-init-flycheck ()
  (dolist (mode '(typescript-mode typescript-tsx-mode))
    (spacemacs/enable-flycheck mode)
    (with-eval-after-load 'flycheck
      (flycheck-add-mode 'javascript-eslint mode))))

(defun madand-typescript/post-init-lsp-mode ()
  (dolist (hook '(typescript-mode-hook typescript-tsx-mode-hook))
    (add-hook hook #'lsp)))

(defun madand-typescript/pre-init-prettier-js ()
  (dolist (mode '(typescript-mode typescript-tsx-mode))
    (add-to-list 'spacemacs--prettier-modes mode))
  (spacemacs/add-to-hooks #'madand-js/maybe-turn-on-prettier-js
                          '(typescript-mode-hook typescript-tsx-mode-hook)
                          'append))

(defun madand-typescript/post-init-smartparens ()
  (let ((sp-mode (if dotspacemacs-smartparens-strict-mode
                     #'smartparens-strict-mode
                   #'smartparens-mode)))
    (spacemacs/add-to-hooks sp-mode '(typescript-mode-local-vars-hook
                                      typescript-tsx-mode-local-vars-hook)
                            'append)))

(defun madand-typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :init
    (progn
      (spacemacs|define-jump-handlers typescript-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "p" 'madand-typescript/open-region-in-playground))))

(defun madand-typescript/post-init-web-mode ()
  ;; Define `typescript-tsx-mode'.
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  ;; Configure `typescript-tsx-mode'.
  (spacemacs|define-jump-handlers typescript-tsx-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
    "p" 'madand-typescript/open-region-in-playground))

(defun madand-typescript/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'madand-typescript//yasnippet-setup
                          '(typescript-mode-hook
                            typescript-tsx-mode-hook)))
