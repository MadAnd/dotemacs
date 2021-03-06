;;; packages.el --- madand-eww layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst madand-eww-packages
  '(
    ace-link
    dash-docs
    (eww :location built-in)
    (eww-imenu :location local)
    (image :location built-in)
    language-detection
    (shr :location built-in)
    window-purpose
    ))

(defun madand-eww/post-init-ace-link ()
  (with-eval-after-load 'ace-link
    (advice-add 'ace-link--eww-action :filter-args
                #'madand-eww//ace-link--eww-action@fix-off-by-one)))

(defun madand-eww/post-init-dash-docs ()
  (with-eval-after-load 'dash-docs
    (setq dash-docs-browser-func #'eww)))

(defun madand-eww/init-eww ()
  (setq eww-bookmarks-directory "~/.spacemacs.d/data/")
  (spacemacs/set-leader-keys
    "awc" 'madand-eww/open-from-clipboard
    "awf" 'madand-eww/eww-open-file
    "awb" 'eww-list-bookmarks
    "aws" 'eww-search-words
    "aww" 'eww)
  ;; Hooks.
  (spacemacs/add-all-to-hook 'eww-mode-hook
                             #'madand//set-text-scale-for-documentation
                             #'visual-line-mode)
  (spacemacs/add-all-to-hook 'eww-after-render-hook
                             #'madand-eww//maybe-turn-on-extra-visual-modes
                             #'recenter)
  ;; Initial evil states for eww modes.
  (dolist (mode '(eww-mode eww-history-mode eww-bookmark-mode eww-buffers-mode))
    (evil-set-initial-state mode 'motion))

  (with-eval-after-load 'eww
    (spacemacs/declare-prefix-for-mode 'eww-mode "ml" "list")
    (spacemacs/declare-prefix-for-mode 'eww-mode "mv" "view")
    (spacemacs/set-leader-keys-for-major-mode 'eww-mode
      "a"  'eww-add-bookmark
      "b"  'eww-back-url
      "d"  'eww-download
      "f"  'eww-forward-url
      "j"  'madand-eww/jump-next-buffer
      "k"  'madand-eww/jump-previous-buffer
      "n"  'eww-next-url
      "h"  'eww-list-histories
      "lb" 'eww-list-buffers
      "lc" 'url-cookie-list
      "lo" 'eww-list-bookmarks
      "p"  'eww-previous-url
      "r"  'eww-reload
      "s"  'browse-web
      "t"  'eww-top-url
      "u"  'eww-up-url
      "vn" 'eww-open-in-new-buffer
      "vx" 'eww-browse-with-external-browser
      "vf" 'eww-toggle-fonts
      "vr" 'eww-readable
      "vs" 'eww-view-source
      "y"  'eww-copy-page-url)
    (evil-define-key 'motion eww-mode-map
      "j"         'evil-next-visual-line
      "k"         'evil-previous-visual-line
      "gj"        'evil-next-line
      "gk"        'evil-previous-line
      "gr"        'eww-reload
      "J"         'madand-eww/jump-next-buffer
      "K"         'madand-eww/jump-previous-buffer
      "s"         'avy-goto-word-1
      "S"         'eww-switch-to-buffer
      (kbd "C-j") 'shr-next-link
      (kbd "C-k") 'shr-previous-link)
    (dolist (keymap (list eww-link-keymap eww-image-link-keymap))
      (define-key keymap "f" 'eww-follow-link)
      (define-key keymap "F" 'eww-open-in-new-buffer))
    (evil-define-key 'motion eww-history-mode-map
      (kbd "RET") 'eww-history-browse
      "f" 'eww-history-browse
      "q" 'quit-window)
    (evil-define-key 'motion eww-bookmark-mode-map
      (kbd "RET") 'eww-bookmark-browse
      "f" 'eww-bookmark-browse
      "dd" 'eww-bookmark-kill
      "p" 'eww-bookmark-yank
      "q" 'quit-window)
    (evil-define-key 'motion eww-buffers-mode-map
      (kbd "RET") 'eww-buffer-select
      "f" 'eww-buffer-select
      "d" 'eww-buffer-kill
      "n" 'eww-buffer-show-next
      "p" 'eww-buffer-show-previous
      "q" 'quit-window)))

(defun madand-eww/init-eww-imenu ()
  (use-package eww-imenu
    :after eww
    :config
    (eww-imenu/register-tag-handlers)))

(defun madand-eww/init-image ()
  (with-eval-after-load 'image
    (define-key image-map "o" nil)
    (define-key image-map "O" 'image-save)))

(defun madand-eww/init-language-detection ()
  (use-package language-detection
    :after eww
    :config
    (add-to-list 'shr-external-rendering-functions
                 '(pre . madand-eww//tag-pre))))

(defun madand-eww/post-init-shr ()
  (with-eval-after-load 'shr
    (setq shr-use-fonts nil
          shr-width 72)))

(defun madand-eww/pre-init-window-purpose ()
  (spacemacs|use-package-add-hook window-purpose
    :pre-config
    (progn
      (add-to-list 'purpose-user-mode-purposes '(eww-mode . eww))
      (purpose-compile-user-configuration))))

;;; packages.el ends here
