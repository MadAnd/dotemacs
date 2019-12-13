;;; packages.el --- madand-eww layer packages file for Spacemacs.
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
    (eww :location built-in)
    language-detection
    ))

(defun madand-eww/init-eww ()
  (spacemacs/add-all-to-hook 'eww-mode-hook
                             #'madand//set-text-scale-for-documentation
                             #'centered-cursor-mode
                             #'madand//eww-turn-on-word-wrapping)
  (spacemacs/add-all-to-hook 'eww-after-render-hook
                             #'madand//eww-maybe-turn-on-centered-buffer-mode
                             #'recenter)
  (dolist (mode '(eww-mode eww-history-mode eww-bookmark-mode eww-buffers-mode))
    (evil-set-initial-state mode 'motion))
  (with-eval-after-load 'eww
    ;; Automatic programming language detection of code snippets.
    (setq shr-external-rendering-functions
          '((pre . madand//eww-tag-pre)))
    (spacemacs/set-leader-keys "aw" 'eww)
    (spacemacs/declare-prefix-for-mode 'eww-mode "ml" "list")
    (spacemacs/declare-prefix-for-mode 'eww-mode "mv" "view")
    (spacemacs/set-leader-keys-for-major-mode 'eww-mode
      "b"  'eww-back-url
      "f"  'eww-forward-url
      "s"  'browse-web
      "r"  'eww-reload
      "p"  'eww-previous-url
      "n"  'eww-next-url
      "h"  'eww-list-histories
      "d"  'eww-download
      "a"  'eww-add-bookmark
      "lb" 'eww-list-buffers
      "lc" 'url-cookie-list
      "lo" 'eww-list-bookmarks
      "t"  'eww-top-url
      "u"  'eww-up-url
      "vx" 'eww-browse-with-external-browser
      "vf" 'eww-toggle-fonts
      "vr" 'eww-readable
      "vs" 'eww-view-source)
    (evil-define-key 'motion eww-mode-map
      "gr"        'eww-reload
      "J"         'madand/eww-jump-next-buffer
      "K"         'madand/eww-jump-previous-buffer
      (kbd "C-j") 'shr-next-link
      (kbd "C-k") 'shr-previous-link
      "o"         'ace-link-eww
      "s"         'avy-goto-char
      "S"         'eww-switch-to-buffer)
    (dolist (keymap (list eww-link-keymap eww-image-link-keymap))
      (define-key keymap "f" 'eww-follow-link)
      (define-key keymap "F" 'eww-open-in-new-buffer))
    (evil-define-key 'motion eww-history-mode-map
      (kbd "RET") 'eww-history-browse
      "f" 'eww-history-browse
      "q" 'kill-this-buffer)
    (evil-define-key 'motion eww-bookmark-mode-map
      (kbd "RET") 'eww-bookmark-browse
      "f" 'eww-bookmark-browse
      "D" 'eww-bookmark-kill
      "y" 'eww-bookmark-yank
      "q" 'kill-this-buffer)
    (evil-define-key 'motion eww-buffers-mode-map
      (kbd "RET") 'eww-buffer-select
      "f" 'eww-buffer-select
      "d" 'eww-buffer-kill
      "n" 'eww-buffer-show-next
      "p" 'eww-buffer-show-previous
      "q" 'kill-this-buffer)))

(defun madand-eww/init-language-detection ()
  (use-package language-detection
    :defer t))

;;; packages.el ends here
