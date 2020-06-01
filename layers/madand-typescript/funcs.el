;;; funcs.el --- madand-typescript layer functions file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; This is a stripped-down and personalized version of the official typescript
;; layer. Mainly all conditional code for different backends was deleted since
;; LSP and PrettierJS is assumed in my setup.

(defun madand-typescript//setup-emmet-mode ()
  "Configure `emmet-mode' for `typescript-mode' buffer."
  (setq-local emmet-expand-jsx-className? t))

(defun madand-typescript//yasnippet-setup ()
  "Activate JS snippets for `typescript-mode'."
  (yas-activate-extra-mode 'js-mode))

(defun madand-typescript/open-region-in-playground (start end)
  "Open selected region in http://www.typescriptlang.org/Playground
If nothing is selected - open the whole current buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (browse-url
   (concat "http://www.typescriptlang.org/Playground#src="
           (url-hexify-string (buffer-substring-no-properties start end)))))
