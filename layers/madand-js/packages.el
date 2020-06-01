;;; packages.el --- madand-js layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

;; This is a stripped-down and personalized version of the official javascript
;; layer. Mainly all conditional code for different backends was deleted since
;; LSP and PrettierJS is assumed in my setup.

;;; Code:

(defconst madand-js-packages
  '(counsel-dash
    dap-mode
    eww
    graphql-mode
    import-js
    (js :location built-in)
    js-doc
    lsp-mode
    prettier-js
    nodejs-repl
    skewer-mode))

(defun madand-js/post-init-counsel-dash ()
  (add-hook 'js-mode-hook #'madand-js/set-js-buffer-dash-docsets)
  (with-eval-after-load 'counsel-dash
    (setq counsel-dash-browser-func #'madand/browse-url-eww-new-buffer)))

(defun madand-js/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'js-mode)
  (add-hook 'js-mode-local-vars-hook #'madand-js//setup-lsp-dap))

(defun madand-js/post-init-eww ()
  (with-eval-after-load 'eww
    (add-to-list 'madand-eww-readable-url-regexp-list "developer\\.mozilla\\.org/")))

(defun madand-js/init-graphql-mode ()
  (use-package graphql-mode
    :defer t))

(defun madand-js/pre-init-import-js ()
  (add-to-list 'spacemacs--import-js-modes '(js-mode . js-mode-hook)))

(defun madand-web/init-js-doc ()
  (evil-define-key 'hybrid js-mode-map "@" 'js-doc-insert-tag))

(defun madand-js/init-js ()
  (use-package js
    :mode ("\\.js\\'" . js-mode)
    :mode ("\\.jsx$" . js-mode)
    :init
    (progn
      (setq js-indent-level 2)
      )
    :config
    (progn
      (evil-define-key 'hybrid js-mode-map
        (kbd "M-e") #'madand-js/insert-comma-before-eol-and-move
        (kbd "RET") #'default-indent-new-line)
      ;; Prefixes
      (spacemacs/declare-prefix-for-mode 'js-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'js-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'js-mode "mr" "refactor"))))

(defun madand-js/init-js-doc ()
  (use-package js-doc
    :hook js-mode-hook
    :init
    (madand-js//js-doc-set-key-bindings 'js-mode)
    :config
    (evil-define-key 'hybrid js-mode-map "@" 'js-doc-insert-tag)))

(defun madand-js/post-init-lsp-mode ()
  (add-hook 'js-mode-hook #'lsp))

(defun madand-js/pre-init-prettier-js ()
  (add-hook 'js-mode-local-vars-hook #'madand-js/maybe-turn-on-prettier-js))

(defun javascript/init-nodejs-repl ()
  (use-package nodejs-repl
    :after js
    :init
    (spacemacs/register-repl 'nodejs-repl
                             'nodejs-repl
                             "nodejs-repl")
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'js-mode "ms" "repl")
      (spacemacs/set-leader-keys-for-major-mode 'js-mode
        "'"  'nodejs-repl
        "se" 'nodejs-repl-send-last-expression
        "sE" 'madand-js/nodejs-send-last-expression-and-focus
        "si" 'madand-js/nodejs-repl-project-root
        "sb" 'nodejs-repl-send-buffer
        "sB" 'madand-js/nodejs-send-buffer-and-focus
        "sl" 'nodejs-repl-send-line
        "sL" 'madand-js/nodejs-send-line-and-focus
        "sr" 'nodejs-repl-send-region
        "sR" 'madand-js/nodejs-send-region-and-focus
        "ss" 'nodejs-repl))))

(defun javascript/init-skewer-mode ()
  (use-package skewer-mode
    :after js
    :init
    (progn
      (spacemacs/register-repl 'skewer-mode
                               'madand-js/skewer-start-repl
                               "skewer")
      (add-hook 'js-mode-hook 'skewer-mode))
    :config
    (progn
      (spacemacs|hide-lighter skewer-mode)
      (spacemacs/set-leader-keys-for-major-mode 'js-mode
        "Se" 'skewer-eval-last-expression
        "SE" 'skewer-eval-print-last-expression
        "Sb" 'skewer-load-buffer
        "SB" 'madand-js/skewer-load-buffer-and-focus
        "Si" 'madand-js/skewer-start-repl
        "Sf" 'skewer-eval-defun
        "SF" 'madand-js/skewer-eval-defun-and-focus
        "Sr" 'madand-js/skewer-eval-region
        "SR" 'madand-js/skewer-eval-region-and-focus
        "Ss" 'skewer-repl))))

;;; packages.el ends here
