;;; packages.el --- madand-ivy layer packages file for Spacemacs.
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

(defconst madand-ivy-packages
  '(counsel
    ivy
    ;; ivy-rich
    swiper
    wgrep))

(defun madand-ivy/post-init-counsel ()
  (with-eval-after-load 'counsel
    (setq counsel-yank-pop-separator (concat "\n" (make-string 80 ?—) "\n"))
    (define-key counsel-find-file-map (kbd "C-u") 'counsel-up-directory)))

(defun madand-ivy/post-init-ivy ()
  (with-eval-after-load 'ivy
    (setq ivy-wrap t
          ivy-height 20)
    (spacemacs/set-leader-keys "hm" #'man)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)))

(defun madand-ivy/init-ivy-rich ()
  (use-package ivy-rich
    :defer t
    :init
    (with-eval-after-load 'ivy
      (ivy-set-display-transformer 'ivy-switch-buffer
                                   'ivy-rich-switch-buffer-transformer))
    :config
    (advice-add 'ivy-rich-switch-buffer-pad :override #'madand/ivy-rich-switch-buffer-pad)))

(defun madand-ivy/post-init-swiper ()
  (with-eval-after-load 'swiper
    (define-key swiper-map (kbd "M-k") 'swiper-mc)))

(defun madand-ivy/post-init-wgrep ()
  (with-eval-after-load 'wgrep
    (setq wgrep-auto-save-buffer t)))

;;; packages.el ends here
