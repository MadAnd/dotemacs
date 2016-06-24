;;; -*- lexical-binding: t -*-
;;; funcs.el --- my-web-dev layer functions file for Spacemacs.
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

(defun evil-php-insert-doc-block ()
  "Insert php-doc block for current function, class or variable."
  (interactive)
  (evil-with-state emacs
    (end-of-line)
    (php-insert-doc-block))
  (evil-append-line nil))

(defun browse-url-eww (url &optional new-window)
  "`browse-url' compatible function to use EWW as a browser."
  (eww url))

(defmacro my-web-dev|inserting-command (str)
  "Return lambda which is an interactive command that inserts STR at point.

Note: this is a workaround for evil-iedit, since straightforward
approach like (define-key map (kbd \"M-d\") \"$\") did not work properly!"
  `(lambda ()
     (interactive)
     (insert ,str)))

(defun my-web-dev/disable-rainbow-identifiers ()
  "Disable `rainbow-identifiers-mode'."
  (rainbow-identifiers-mode -1))

(defun my-web-dev/toggle-php-web-mode ()
  "Toggle between `php-mode' and `web-mode'."
  (interactive)
  (if (eq major-mode 'web-mode)
      (php-mode)
    (web-mode)))

(defun my-web-dev/set-fill-column ()
  "If value of `my-web-dev-php-fill-column' is not nil, set fill column to that
value."
  (interactive)
  (when my-web-dev-php-fill-column
    (set-fill-column my-web-dev-php-fill-column)))

(defun my-web-dev/php-after-save ()
  "Regenerate PHP classes cache for the current projectile project."
  ;; (php-helpers//load-php-class-list (projectile-project-root) t)
  )

(defun my-web-dev/delayed-rainbow-identifiers-mode ()
  "Disable and then, after a delay, enable `rainbow-identifiers-mode'.

This is a workaround for colorized text inside of comments in some major modes,
like `php-mode' and `js2-mode.'"
  (rainbow-identifiers-mode -1)
  (let ((current-buffer (current-buffer)))
    (run-with-idle-timer 1 nil (lambda ()
                                 (with-current-buffer current-buffer
                                   (rainbow-identifiers-mode 1))))))

;;; funcs.el ends here
