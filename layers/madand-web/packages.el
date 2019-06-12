;;; packages.el --- madand-web layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2018 Andriy Kmit'
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:


;;; Code:

(defconst madand-web-packages
  '(company-php
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
    web-mode
    web-beautify
    yaml-mode
    (drupal-mode :excluded t)))

(defun madand-web/post-init-company-php ()
  (pop company-backends-php-mode)
  (push '(company-ac-php-backend company-keywords company-dabbrev-code)
        company-backends-php-mode))

(defun madand-web/init-geben ()
  (use-package geben
    :after php-mode
    :init
    (progn
      (spacemacs/set-leader-keys
        "Gg" #'geben
        "Gp" #'geben-toggle-pause-at-entry-line-flag
        "Gr" #'geben-run
        "GR" #'geben-run-to-cursor
        "Gs" #'geben-stop
        "Gq" #'geben-end
        "Gf" #'geben-find-file
        "GF" #'geben-open-file
        "Gj" #'geben-step-again
        "Gk" #'geben-step-into
        "Gh" #'geben-step-over
        "Gl" #'geben-step-out
        "Gt" #'geben-show-backtrace
        "Gc" #'geben-display-context
        "Ga" #'geben-set-breakpoint-line
        "Gd" #'geben-set-breakpoint-call
        "GD" #'geben-set-breakpoint-return
        "Ge" #'geben-set-breakpoint-exception
        "GA" #'geben-unset-breakpoint-line
        "Gx" #'geben-clear-breakpoints)
      (spacemacs|define-transient-state geben
        :title "Geben Transient State"
        :doc "\n
 Breakpoints^^     Steps^^      Actions^^             Show^^
 ───────────^^──── ─────^^───── ───────^^──────────── ────^^──────────
 [_d_] call        [_j_] again  [_r_] run             [_t_] backtrace
 [_D_] return      [_k_] over   [_s_] stop            [_c_] context
 [_e_] exception   [_h_] into   [_p_] pause at entry
 [_x_] clear       [_l_] out    [_q_] end\n"
        :bindings
        ("r" #'geben-run)
        ("s" #'geben-stop :exit t)
        ("p" #'geben-toggle-pause-at-entry-line-flag)
        ("q" #'geben-end :exit t)
        ("j" #'geben-step-again)
        ("k" #'geben-step-over)
        ("h" #'geben-step-into)
        ("l" #'geben-step-out)
        ("t" #'geben-show-backtrace)
        ("c" #'geben-display-context :exit t)
        ("d" #'geben-set-breakpoint-call)
        ("D" #'geben-set-breakpoint-return)
        ("e" #'geben-set-breakpoint-exception)
        ("C" #'geben-set-breakpoint-condition)
        ("x" #'geben-clear-breakpoints)
        :on-enter
        (geben 1))
      (spacemacs/set-leader-keys "G." #'spacemacs/geben-transient-state/body))
    :config
    (setq geben-display-window-function #'popwin:display-buffer
          geben-temporary-file-directory (concat spacemacs-cache-directory "geben/")
          geben-path-mappings  '(("/home/madand/dev/php/rentling/app" "/app")
                                 ("/home/madand/dev/php/tkanaua" "/app")))))


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
      (kbd "RET") (kbd "M-j"))))


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
      ;; (setq nodejs-repl-arguments '("--require" "babel-register"))
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
    :after php-mode
    :commands php-insert-doc-block
    :init
    (setq php-insert-doc-access-tag nil
          php-insert-doc-attribute-tags nil
          php-insert-doc-uses-tag nil
          php-insert-doc-varname-on-var nil
          php-insert-doc-copyright-name nil
          php-insert-doc-copyright-email nil
          php-insert-doc-author-name "Andiy Kmit'"
          php-insert-doc-author-email "dev@madand.net")
    (spacemacs/set-leader-keys-for-major-mode 'php-mode
      "id" #'madand/php-insert-doc-block)))

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
  (spacemacs/declare-prefix-for-mode 'php-mode "mi" "insert")

  (add-hook 'php-mode-hook #'madand/set-fill-column t))

(defun madand-web/pre-init-phpcbf ()
  (with-eval-after-load 'phpcbf
    (setq phpcbf-standard "PSR2")))

(defun madand-web/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "=" #'madand/js-standard-fix-file))

(defun madand-web/post-init-web-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    "w" #'madand/toggle-php-web-mode)
  (add-to-list 'auto-mode-alist
               '("/\\(views\\|common/mail\\)/.*\\.php\\'" . web-mode)))

(defun madand-web/post-init-yaml-mode ()
  (with-eval-after-load 'yaml-mode
    (setq yaml-indent-offset 4)))

;;; packages.el ends here
