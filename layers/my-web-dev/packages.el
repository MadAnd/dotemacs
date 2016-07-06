;;; packages.el --- my-web-dev layer packages file for Spacemacs.
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

(defconst my-web-dev-packages
  '(
    flycheck
    js2-mode
    (php-doc :location local)
    (php-helpers :location local)
    php-mode
    php-refactor-mode
    web-mode
    ))

(defun my-web-dev/post-init-flycheck ()
  (setq flycheck-phpcs-standard "PSR2"
        flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs"))

(defun my-web-dev/post-init-js2-mode ()
  (setq js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook #'my-web-dev/delayed-rainbow-identifiers-mode)
  )

(defun my-web-dev/init-php-doc ()
  (use-package php-doc
    :commands php-insert-doc-block
    :init
    (setq-default php-insert-doc-access-tag nil
                  php-insert-doc-attribute-tags nil
                  php-insert-doc-uses-tag nil
                  php-insert-doc-varname-on-var nil
                  php-insert-doc-copyright-name nil
                  php-insert-doc-copyright-email nil
                  php-insert-doc-author-name "Andiy Kmit'"
                  php-insert-doc-author-email "dev@madand.net")))

(defun my-web-dev/init-php-helpers ()
  (use-package php-helpers
    :defer t)
  (with-eval-after-load 'php-mode
    (require 'php-helpers)
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "iu" #'php-helpers/do-insert-use-class
      "iU" #'php-helpers/do-insert-use-class-region-or-symbol
      "ic" #'php-helpers/insert-class
      "in" #'php-helpers/go-to-namespace)
    (add-hook 'php-mode-hook
              (lambda ()
                (add-hook 'after-save-hook #'my-web-dev/php-after-save nil t)))))

(defun my-web-dev/post-init-php-mode ()
  (defun php-search-web-documentation-in-default-browser (word)
    "Search documentation on PHP website in the default browser.

The value of `php-search-documentation-browser-function' will be ignored,
if set, and `browse-url-browser-function' will be used instead."
    (interactive (php--search-documentation-read-arg))
    (let ((php-search-documentation-browser-function nil))
      (php-search-web-documentation word)))

  (setq-default php-template-compatibility nil
                php-manual-path "/usr/share/doc/php/php-chunked-xhtml"
                php-search-documentation-browser-function #'browse-url-eww
                php-mode-coding-style 'psr2
                )

  ;; Shortcuts for common tokens.
  (evil-define-key 'hybrid php-mode-map
    (kbd "C-s") "->"
    (kbd "C--") " => "
    (kbd "C-'") "::"
    (kbd "M-o") "<ESC>$i;")

  (spacemacs/declare-prefix-for-mode 'php-mode "mh" "documentation")
  (spacemacs/set-leader-keys-for-major-mode 'php-mode
    "hh" #'php-search-documentation
    "hH" #'php-search-web-documentation-in-default-browser
    "w" #'my-web-dev/toggle-php-web-mode)

  (when (configuration-layer/package-usedp 'php-doc)
    (spacemacs/declare-prefix-for-mode 'php-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "id" #'evil-php-insert-doc-block))

  (add-hook 'php-mode-hook #'my-web-dev/set-fill-column t)
  (add-hook 'php-mode-hook #'my-web-dev/delayed-rainbow-identifiers-mode))

(defun my-web-dev/pre-init-phpcbf ()
  (spacemacs|use-package-add-hook phpcbf
    :post-config
    (setq phpcbf-standard "PSR2")))

(defun my-web-dev/post-init-web-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    "w" #'my-web-dev/toggle-php-web-mode)

  (add-to-list 'auto-mode-alist
               '("/\\(views\\|common/mail\\)/.*\\.php\\'" . web-mode))

  (add-hook 'web-mode-hook #'my-web-dev/disable-rainbow-identifiers))

(defun my-web-dev/init-php-refactor-mode ()
  (use-package php-refactor-mode
    :defer t
    :diminish php-refactor-mode
    :init
    (progn
      (setq
       php-refactor-command (expand-file-name "~/.composer/vendor/bin/refactor"))

      (dolist (prefix '(("mr" . "refactor")
                        ("mrl" . "local variable")
                        ("mrr" . "rename")
                        ("mre" . "extract")
                        ("mro" . "optimize")))
        (spacemacs/declare-prefix-for-mode 'php-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'php-mode
        "rli" #'php-refactor--convert-local-to-instance-variable
        "rrv" #'php-refactor--rename-local-variable
        "rem" #'php-refactor--extract-method
        "rou" #'php-refactor--optimize-use)

      (add-hook 'php-mode-hook #'php-refactor-mode))))

;;; packages.el ends here
