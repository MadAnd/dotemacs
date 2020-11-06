;;; packages.el --- madand-lisp layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst madand-lisp-packages
  '(
    ace-link
    auto-highlight-symbol
    (elisp-mode :location built-in)
    (evil-cleverparens :excluded t)
    (eww :location built-in)
    (lisp-mode :location built-in)
    lispy
    lispyville
    projectile
    evil
    company
    smartparens
    (sly :requires smartparens)
    (sly-macrostep :requires (sly macrostep))
    (sly-repl-ansi-color :requires sly)
    xterm-color
    ))

(defun madand-lisp/post-init-ace-link ()
  (with-eval-after-load 'sly
    (dolist (map '(sly-trace-dialog-mode-map
                   sly-inspector-mode-map))
      (evil-define-key 'evilified map "o" #'ace-link-help))))

(defun madand-lisp/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'lisp-mode)))

(defun madand-lisp/init-elisp-mode ()
  (add-hook 'emacs-lisp-mode-hook #'madand-lisp/clear-default-jump-handlers -99)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  (add-hook 'emacs-lisp-mode-hook #'madand-lisp//yasnippet-configure-h)

  (with-eval-after-load 'elisp-mode
    (madand/elisp-setup-flet-indent)))

(defun madand-lisp/post-init-eww ()
  (add-to-list 'madand-eww-no-images-url-regexp-list "HyperSpec/"))

(defun madand-lisp/init-lisp-mode ()
  (add-hook 'lisp-mode-hook #'madand-lisp/clear-default-jump-handlers -99)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (add-hook 'lisp-mode-hook #'lispyville-mode)
  (add-hook 'lisp-mode-hook #'madand-lisp//yasnippet-configure-h)
  (with-eval-after-load 'lisp-mode
    (put 'uiop:define-package 'common-lisp-indent-function '(as defpackage))))

(defun madand-lisp/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (spacemacs|diminish lispy-mode)
    :config
    ;; Make it possible to enter qualified symbols like cl:and in non-operator
    ;; position like (boundp cl:and).
    (setq lispy-colon-no-space-regex '((lisp-mode . "[^\\)]")
                                       (sly-mrepl-mode . "[^\\)]")))))

(defun madand-lisp/init-lispyville ()
  (use-package lispyville
    :defer t
    :init
    (spacemacs|diminish lispyville-mode)))

(defun madand-lisp/post-init-projectile ()
  (with-eval-after-load 'projectile
    (advice-add 'projectile-find-implementation-or-test :around
                #'madand/projectile-find-implementation-or-test--around)
    (projectile-register-project-type
     'common-lisp #'madand/project-type-common-lisp-p
     :test-dir "tests/")))

(defun madand-lisp/pre-init-evil ()
  (when (configuration-layer/package-used-p 'sly)
    (madand/advice-add@evil-fix-point-off-by-one 'sly-last-expression)))

(defun madand-lisp/pre-init-smartparens ()
  (with-eval-after-load 'smartparens
    (when (configuration-layer/package-used-p 'sly)
      (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
      (sp-local-pair '(sly-mrepl-mode) "`" "`" :actions nil))))

(defun madand-lisp/init-sly ()
  (use-package sly
    :defer t
    :init
    (progn
      (setq sly-net-coding-system 'utf-8-unix)
      (spacemacs/register-repl 'sly 'sly)
      ;; Setup initial evil states for various sly modes.
      (evil-set-initial-state 'sly-mrepl-mode 'normal)
      (dolist (mode '(sly-db-mode
                      sly-inspector-mode
                      sly-trace-dialog-mode
                      sly-stickers--replay-mode
                      sly-xref-mode))
        (evil-set-initial-state mode 'motion))
      (dolist (contrib '(sly-scratch))
        (add-to-list 'sly-contribs contrib))
      ;; Hyperspec setup.
      (setq common-lisp-hyperspec-root
            (concat "file://" (expand-file-name "~/docs/CommonLisp/HyperSpec/")))
      (dolist (fun '(common-lisp-hyperspec
                     common-lisp-hyperspec-lookup-reader-macro
                     common-lisp-hyperspec-format
                     common-lisp-hyperspec-glossary-term))
        (advice-add fun :around #'madand-lisp//with-browse-url-in-eww))
      (spacemacs|define-jump-handlers lisp-mode sly-edit-definition)
      (spacemacs|define-jump-handlers sly-mrepl-mode
                                      sly-edit-definition-other-window)

      (spacemacs|define-transient-state common-lisp-navigation
        :title "Common Lisp Navigation Transient State"
        :doc "
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition

[_q_] Exit"
        :foreign-keys run
        :bindings
        ("g" sly-edit-definition)
        ("G" sly-edit-definition-other-window)
        ("b" sly-pop-find-definition-stack)
        ("n" sly-next-note)
        ("N" sly-previous-note)
        ("s" sly-stickers-next-sticker)
        ("S" sly-stickers-prev-sticker)
        ("q" nil :exit t)))
    :config
    (progn
      (setq sly-lisp-implementations
            '((ros-sbcl ("ros" "-L" "sbcl-bin" "run"))
              (ros-ccl  ("ros" "-L" "ccl"      "run"))
              (ros-ecl  ("ros" "-L" "ecl"      "run"))
              (abcl ("/home/madand/.roswell/impls/x86-64/linux/abcl-bin/1.7.1/abcl")))
            sly-default-lisp 'ros-sbcl
            sly-description-autofocus t
            sly-command-switch-to-existing-lisp 'never
            sly-auto-select-connection 'always
            sly-kill-without-query-p t)
      (add-hook 'sly-popup-buffer-mode-hook
                #'spacemacs/toggle-rainbow-identifier-off)
      (add-hook 'sly-mode-hook #'madand-lisp//turn-off-sly-symbol-completion-mode)
      (add-hook 'sly-mrepl-mode-hook #'madand-lisp//yasnippet-configure-h)
      (spacemacs/add-all-to-hook 'sly-mrepl-mode-hook
                                 #'yas-minor-mode
                                 #'madand-lisp//yas-activate-lisp-snippets-h
                                 #'lispy-mode
                                 #'lispyville-mode)
      (advice-add 'sly-show-description :override #'madand/sly-show-description)
      (advice-add 'sly-start :around #'madand-lisp//sly-start@maybe-use-qlot))))

(defun madand-lisp/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes sly-mode sly-mrepl-mode)
  (with-eval-after-load 'counsel
    (advice-add 'counsel--company-display-transformer :around
                #'madand//counsel--company-display-transformer--sly)))

(defun madand-lisp/init-sly-macrostep ()
  (use-package sly-macrostep
    :after sly
    :config
    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "ms" 'spacemacs/macrostep-transient-state/body))))

(defun madand-lisp/init-sly-repl-ansi-color ()
  (cl-pushnew 'sly-repl-ansi-color sly-contribs)
  (use-package sly-repl-ansi-color
    :defer t))

(defun madand-lisp/pre-init-xterm-color ()
  (when (configuration-layer/package-usedp 'sly)
    (add-hook 'sly-mrepl-mode-hook #'madand-lisp/xterm-preserve-colors)))

;;; packages.el ends here
