;;; -*- lexical-binding: t -*-
;;; funcs.el --- madand-web layer functions file for Spacemacs.
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

(defun madand/php-insert-doc-block ()
  "Insert php-doc block for current function, class or variable."
  (interactive)
  (evil-with-state emacs
    (end-of-line)
    (php-insert-doc-block))
  (evil-append-line nil))

(defun madand/browse-url-eww (url &optional new-window)
  "`browse-url' compatible function to use EWW as a browser."
  (eww url))

(defmacro madand|gen-insert-command (str)
  "Return lambda which is an interactive command that inserts STR at point.

Note: this is a workaround for evil-iedit, since straightforward
approach like (define-key map (kbd \"M-d\") \"$\") did not work properly!"
  `(lambda ()
     (interactive)
     (insert ,str)))

(defun madand/disable-rainbow-identifiers ()
  "Disable `rainbow-identifiers-mode'."
  (rainbow-identifiers-mode -1))

(defun madand/toggle-php-web-mode ()
  "Toggle between `php-mode' and `web-mode'."
  (interactive)
  (if (eq major-mode 'web-mode)
      (php-mode)
    (web-mode))
  (hack-dir-local-variables)
  (hack-local-variables-apply))

(defun madand/set-fill-column ()
  "If value of `madand-web-php-fill-column' is not nil, set fill column to that
value."
  (interactive)
  (when madand-web-php-fill-column
    (set-fill-column madand-web-php-fill-column)))

(defun madand/php-after-save ()
  "Regenerate PHP classes cache for the current projectile project."
  ;; (php-helpers//load-php-class-list (projectile-project-root) t)
  )

(defun madand/php-search-web-documentation-in-default-browser (word)
  "Search documentation on PHP website in the default browser.

The value of `php-search-documentation-browser-function' will be ignored,
if set, and `browse-url-browser-function' will be used instead."
  (interactive (php--search-documentation-read-arg))
  (let ((php-search-documentation-browser-function nil))
    (php-search-web-documentation word)))



(defun madand/nodejs-repl-project-root ()
  "Start NodeJS REPL. If inside a project, change the Node process working dir
to the project root."
  (interactive)
  (nodejs-repl-switch-to-repl)
  (when (projectile-project-p)
    (nodejs-repl--send-string
     (format "process.chdir('%s')\n" (projectile-project-root))))
  (evil-insert-state))

(defun madand/nodejs-repl-send-region-and-focus (beg end)
  "Send region to NodeJS REPL and focus the REPL buffer."
  (interactive "r")
  (nodejs-repl-send-region beg end)
  (nodejs-repl-switch-to-repl)
  (evil-insert-state))

(defun madand/nodejs-repl-send-buffer-and-focus ()
  "Send buffer to NodeJS REPL and focus the REPL buffer."
  (interactive)
  (nodejs-repl-send-buffer)
  (nodejs-repl-switch-to-repl)
  (evil-insert-state))

(defun madand/nodejs-repl-load ()
  "Load file repl.js form project root, if any."
  (interactive)
  (let ((repl-file (concat (projectile-project-root) "repl.js")))
    (if (file-exists-p repl-file)
        (nodejs-repl-load-file repl-file)
      (nodejs-repl-switch-to-repl))))

(defun madand/register-nodejs-repl-bindings ()
  "Register `nodejs-repl' keybindings."
  (interactive)
  (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "nodejs")
  (spacemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "'" 'madand/nodejs-repl-project-root
    "ee" nil
    "eE" nil
    "sb" 'nodejs-repl-send-buffer
    "sB" 'madand/nodejs-repl-send-buffer-and-focus
    "si" 'madand/nodejs-repl-load
    "sf" nil
    "sF" nil
    "sr" 'nodejs-repl-send-region
    "sR" 'madand/nodejs-repl-send-region-and-focus
    "ss" 'madand/nodejs-repl-project-root))

(defun madand/register-skewer-repl-bindings ()
  "Register `nodejs-repl' keybindings."
  (interactive)
  (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "skewer")
  (spacemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "'" 'spacemacs/skewer-start-repl
    "ee" 'skewer-eval-last-expression
    "eE" 'skewer-eval-print-last-expression
    "sb" 'skewer-load-buffer
    "sB" 'spacemacs/skewer-load-buffer-and-focus
    "si" 'spacemacs/skewer-start-repl
    "sf" 'skewer-eval-defun
    "sF" 'spacemacs/skewer-eval-defun-and-focus
    "sr" 'spacemacs/skewer-eval-region
    "sR" 'spacemacs/skewer-eval-region-and-focus
    "ss" 'skewer-repl))


(defvar madand-web-js-repl 'skewer "REPL backend to be used in JavaScript buffers.

Valid values are: nodejs and skewer.")

(defun madand/toggle-skewer-and-nodejs-repl ()
  "Toggle between `skewer-mode' and `nodejs-repl' keybindings."
  (interactive)
  (if (eq 'skewer madand-web-js-repl)
      (progn
        (madand/register-nodejs-repl-bindings)
        (setq madand-web-js-repl 'nodejs))
    (madand/register-skewer-repl-bindings)
    (setq madand-web-js-repl 'skewer)))



(defun madand/js-standard-fix-file ()
  "Run 'standard --fix' on the current file."
  (interactive)
  (let ((file (buffer-file-name))
        (standard-js-program "standard"))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (save-buffer)
    (when (zerop
           (call-process standard-js-program nil nil nil "--fix" file))
      ;; (fundamental-mode)
      (revert-buffer t t t)
      (js2-mode))))

(defun madand//projectile-test-suffix (current-suffix-function)
  "Custom test suffix function adding support of .test.js files
for nodeapp project type.

Fall back to CURRENT-SUFFIX-FUNCTION for other project types."
  (lambda (project-type)
    (cond
     ((member project-type '(madand-nodeapp)) ".test")
     (t (funcall current-suffix-function project-type)))))

;;; funcs.el ends here
