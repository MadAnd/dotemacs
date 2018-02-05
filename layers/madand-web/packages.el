;;; packages.el --- madand-web layer packages file for Spacemacs.
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

(defconst madand-web-packages
  '(company-php
    ;; ede-php-autoload
    flycheck
    geben
    graphql-mode
    js2-mode
    js-doc
    mocha
    nodejs-repl
    popwin
    (php-doc :location local)
    (php-helpers :location local)
    (php-extras :excluded t)
    php-mode
    php-refactor-mode
    ;; projectile
    web-mode
    web-beautify
    (drupal-mode :excluded t)))

(defun madand-web/post-init-company-php ()
  (pop company-backends-php-mode)
  (push '(company-ac-php-backend company-keywords company-dabbrev-code)
        company-backends-php-mode))

(defun madand-web/init-ede-php-autoload ()
  (use-package ede-php-autoload
    :defer t
    :after php-mode)
  (add-hook 'php-mode-hook #'ede-php-autoload-mode))

(defun madand-web/init-geben ()
  (use-package geben
    :defer t
    :after php-mode
    :config
    (progn
      (setq geben-display-window-function 'popwin:display-buffer
            geben-path-mappings  '(("/home/madand/dev/php/rentling/app" "/app")))
      (evil-set-initial-state 'geben-mode 'emacs))))

(defun madand-web/post-init-flycheck ()
  (setq flycheck-phpcs-standard "PSR2"
        flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs"))

(defun madand-web/init-graphql-mode ()
  (use-package graphql-mode
    :defer t))

(defun madand-web/post-init-js2-mode ()
  (with-eval-after-load 'js2-mode
    (setq js2-basic-offset 2
          js2-strict-missing-semi-warning nil
          js2-strict-inconsistent-return-warning nil)
    ;; Shortcuts for common yet cumbersome things.
    (evil-define-key 'hybrid js2-mode-map
      (kbd "M-o") (kbd "C-o $;")
      (kbd "M-e") (kbd "C-o $,")
      (kbd "RET") (kbd "M-j")))

  (add-hook 'js2-mode-hook #'madand/disable-rainbow-identifiers-mode)

  ;; (spacemacs|define-micro-state string-inflection
  ;;   :doc "Press [_i_] to cycle through available inflections."
  ;;   :execute-binding-on-enter t
  ;;   :use-minibuffer t
  ;;   :evil-leader "xi"
  ;;   :persistent t
  ;;   :bindings
  ;;   ("i" madand/cameldash-variable-at-point))
  )

(defun madand-web/post-init-js-doc ()
  (evil-define-key 'hybrid js2-mode-map "@" 'js-doc-insert-tag))

(defun madand-web/init-mocha ()
  (use-package mocha
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode "mt" "test")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "tt" #'mocha-test-file
        "tT" #'mocha-test-at-point
        "tp" #'mocha-test-project))))

(defun madand-web/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :init
    (progn
      (setq nodejs-repl-arguments '("--require" "babel-register"))
      (spacemacs/register-repl 'nodejs-repl
                               'nodejs-repl
                               "nodejs")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "st" #'madand/toggle-skewer-and-nodejs-repl))))

(defun madand-web/post-init-popwin ()
  (with-eval-after-load 'popwin
    (push
     '(".*backtrace\*" :regexp t :position bottom)
     popwin:special-display-config)
    (push
     '(".*context\*" :regexp t :position left :stick t :noselect t :width 80)
     popwin:special-display-config)))

(defun madand-web/init-php-doc ()
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
  (setq-default php-template-compatibility nil
                php-manual-path "/usr/share/doc/php/php-chunked-xhtml"
                php-search-documentation-browser-function #'madand/browse-url-eww
                php-mode-coding-style 'psr2
                )

  (evil-define-key 'hybrid c-mode-map (kbd "RET") (kbd "M-j"))
  ;; Shortcuts for common yet cumbersome things.
  (evil-define-key 'hybrid php-mode-map
    (kbd "C-s") "->"
    (kbd "C--") " => "
    (kbd "C-'") "::"
    (kbd "M-o") (kbd "C-o $;")
    (kbd "M-u") (kbd "C-o $,"))
  (evil-define-key '(normal motion) php-mode-map
    "{" #'php-beginning-of-defun
    "}" #'php-end-of-defun
    "(" #'c-beginning-of-statement
    ")" #'c-end-of-statement)

  (spacemacs/declare-prefix-for-mode 'php-mode "mh" "documentation")
  (spacemacs/set-leader-keys-for-major-mode 'php-mode
    "hh" #'php-search-documentation
    "hH" #'madand/php-search-web-documentation-in-default-browser
    "w" #'madand/toggle-php-web-mode)

  (when (configuration-layer/package-usedp 'php-doc)
    (spacemacs/declare-prefix-for-mode 'php-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "id" #'madand/php-insert-doc-block))

  (add-hook 'php-mode-hook #'madand/set-fill-column t)
  (add-hook 'php-mode-hook #'madand/disable-rainbow-identifiers-mode))

(defun madand-web/pre-init-phpcbf ()
  (spacemacs|use-package-add-hook phpcbf
    :post-config
    (setq phpcbf-standard "PSR2")))

(defun madand-web/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "=" #'madand/js-standard-fix-file))

(defun madand-web/post-init-web-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    "w" #'madand/toggle-php-web-mode)

  (add-to-list 'auto-mode-alist
               '("/\\(views\\|common/mail\\)/.*\\.php\\'" . web-mode))

  (add-hook 'web-mode-hook #'madand/disable-rainbow-identifiers))

(defun madand-web/post-init-projectile ()
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'madand-nodeapp nil
                                      :compile "npm run build"
                                      :test "npm run test"
                                      :run "npm run start")
    (setq projectile-test-suffix-function
          (madand//projectile-test-suffix projectile-test-suffix-function))))

(defun madand-web/init-php-refactor-mode ()
  (use-package php-refactor-mode
    :defer t
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

      (add-hook 'php-mode-hook #'php-refactor-mode))
    :config
    (spacemacs|hide-lighter php-refactor-mode)))

;;; packages.el ends here
