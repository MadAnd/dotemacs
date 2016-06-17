;;; packages.el --- madand-lisp layer packages file for Spacemacs.
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

;; My lisp related configuration.

;;; Code:

(defconst madand-lisp-packages
  '(
    cider
    ;; clojure-cheatsheet
    clojure-mode
    (emacs-lisp :location built-in)
    evil-cleverparens
    ))

(defun madand-lisp/post-init-cider ()
  (setq cider-repl-display-help-banner nil))

(defun madand-lisp/post-init-clojure-mode ()
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode))

(defun madand-lisp/post-init-emacs-lisp ()
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

(defun madand-lisp/post-init-evil-cleverparens ()
  (setq evil-cleverparens-swap-move-by-word-and-symbol t)

  (with-eval-after-load 'evil-cleverparens
    (evil-define-key 'normal evil-cleverparens-mode-map
      ;; I use s and S for Avy.
      "s" nil
      "S" nil
      ;; TODO: delete these when compatibility with `evil-surround' is fixed.
      "d" nil
      "c" nil
      "y" nil)))

;;; packages.el ends here
