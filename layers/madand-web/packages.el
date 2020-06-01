;;; packages.el --- madand-web layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst madand-web-packages
  '(company
    counsel-dash
    eww
    flycheck
    lsp-mode
    (php-doc :location local)
    (php-helpers :location local)
    php-mode
    web-mode
    yaml-mode
    ;; Exclude some packages defined by the official php layer.
    (company-php :excluded t)
    (drupal-mode :excluded t)
    (php-auto-yasnippets :excluded t)
    (php-extras :excluded t)))

(defun madand-web/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes php-mode
    :append-hooks nil
    :call-hooks t))

(defun madand-web/post-init-eww ()
  (with-eval-after-load 'eww
    (add-to-list 'madand-eww-readable-url-regexp-list "developer\\.mozilla\\.org/")))

(defun madand-web/post-init-flycheck ()
  (setq flycheck-phpcs-standard "PSR2"
        flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs"))

(defun madand-web/post-init-lsp-mode ()
  (add-hook 'php-mode-hook #'lsp)
  (with-eval-after-load 'lsp-mode
    (setq lsp-file-watch-threshold 100000)
    (evil-define-key 'normal php-mode-map "gd" #'lsp-ui-peek-find-definitions)))

(defun madand-web/init-php-doc ()
  (use-package php-doc
    :after php-mode
    :commands php-insert-doc-block
    :init
    (progn
      (setq php-insert-doc-access-tag nil
            php-insert-doc-attribute-tags nil
            php-insert-doc-uses-tag nil
            php-insert-doc-varname-on-var nil
            php-insert-doc-copyright-name nil
            php-insert-doc-copyright-email nil
            php-insert-doc-author-name "Andriy Kmit"
            php-insert-doc-author-email "dev@madand.net")
      (spacemacs/set-leader-keys-for-major-mode 'php-mode
        "id" #'madand/php-insert-doc-block))))

(defun madand-web/init-php-helpers ()
  (use-package php-helpers
    :defer t)
  (with-eval-after-load 'php-mode
    (require 'php-helpers)
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "iu" #'php-helpers/do-insert-use-class-region-or-symbol
      "iU" #'php-helpers/do-insert-use-class
      "ic" #'php-helpers/insert-class
      "in" #'php-helpers/go-to-namespace
      "iv" #'php-helpers/insert-var-doc-comment)
    (evil-define-key 'hybrid php-mode-map
      (kbd "C-c iu") #'php-helpers/do-insert-use-class-region-or-symbol
      (kbd "C-c ic") #'php-helpers/insert-class
      (kbd "C-c iv") #'php-helpers/insert-var-doc-comment)
    (add-hook 'php-mode-hook #'php-helpers/register-sort-uses-after-save)
    (add-hook 'php-mode-hook
              (lambda ()
                (add-hook 'after-save-hook #'madand/php-after-save nil t)))))

(defun madand-web/post-init-php-mode ()
  (with-eval-after-load 'php-mode
    (setq php-template-compatibility nil
          php-manual-path "/usr/share/doc/php/php-chunked-xhtml"
          php-search-documentation-browser-function #'madand/browse-url-eww
          php-mode-coding-style 'psr2)
    (evil-define-key 'hybrid c-mode-map (kbd "RET") (kbd "M-j"))
    ;; Shortcuts for common yet cumbersome things.
    (evil-define-key 'hybrid php-mode-map
      (kbd "C-s") "->"
      (kbd "C--") " => "
      (kbd "C-'") "::"
      (kbd "C-;") "\\"
      (kbd "M-o") (kbd "C-o $;")
      (kbd "M-u") (kbd "C-o $,"))
    (evil-define-key '(normal motion) php-mode-map
      "{" #'php-beginning-of-defun
      "}" #'php-end-of-defun
      "(" #'c-beginning-of-statement
      ")" #'c-end-of-statement)
    ;; Workaround for parens and braces no more pairing in Emacs 27
    (define-key php-mode-map "(" #'self-insert-command)
    (define-key php-mode-map ")" #'self-insert-command)
    (spacemacs/declare-prefix-for-mode 'php-mode "mh" "documentation")
    (spacemacs/declare-prefix-for-mode 'php-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "hh" #'php-search-documentation
      "hH" #'madand/php-search-web-documentation-in-default-browser
      "w" #'madand/toggle-php-web-mode)
    (add-hook 'php-mode-hook #'madand-web//set-php-buffer-dash-docsets)
    (add-hook 'php-mode-hook #'madand/php-set-fill-column -90)
    (add-hook 'php-mode-hook #'madand/set-tab-width-4 -90)
    (add-hook 'php-mode-hook #'madand/remove-dumb-jump-from-jump-handlers 90)))

(defun madand-web/pre-init-phpcbf ()
  (with-eval-after-load 'phpcbf
    (setq phpcbf-standard "PSR2")))

(defun madand-web/post-init-web-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    "w" #'madand/toggle-php-web-mode)
  (add-to-list 'auto-mode-alist
               '("/\\(views\\|common/mail\\)/.*\\.php\\'" . web-mode)))

(defun madand-web/post-init-yaml-mode ()
  (with-eval-after-load 'yaml-mode
    (setq yaml-indent-offset 4)))

;;; packages.el ends here
