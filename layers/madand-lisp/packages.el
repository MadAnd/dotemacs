;;; packages.el --- madand-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst madand-lisp-packages
  '(cider
    clj-refactor
    slime))

(defun madand-lisp/post-init-cider ()
  (with-eval-after-load 'cider
    (setq cider-repl-display-help-banner nil
          cider-offer-to-open-cljs-app-in-browser nil)
    (dolist (m '(clojure-mode
                 clojurec-mode
                 clojurescript-mode
                 clojurex-mode
                 cider-repl-mode
                 cider-clojure-interaction-mode))
      (spacemacs/set-leader-keys-for-major-mode m
        "el" #'madand/cider-eval-sexp-end-of-line)
      (spacemacs|hide-lighter cider-mode))))

(defun madand-lisp/post-init-clj-refactor ()
  (with-eval-after-load 'clj-refactor
    (spacemacs|hide-lighter clj-refactor-mode)))

(defun madand-lisp/post-init-slime ()
  (with-eval-after-load 'slime
    (setq inferior-lisp-program "ros -Q run"
          slime-description-autofocus t)
    (advice-add 'slime-show-description
                :override #'madand/slime-show-description)))

;;; packages.el ends here
