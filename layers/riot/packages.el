;;; packages.el --- riot Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq riot-packages
      '(
        evil-matchit
        flycheck
        js-doc
        smartparens
        web-beautify
        web-mode
        ))

(defun riot/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'riot-mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                            (evilmi-javascript-get-tag evilmi-javascript-jump)
                                            (evilmi-html-get-tag evilmi-html-jump)))))

(defun riot/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'riot-mode)))
  (defun riot/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-eslint (executable-find "eslint"))
           (local-eslint (expand-file-name "node_modules/.bin/eslint"
                                           root))
           (eslint (if (file-executable-p local-eslint)
                       local-eslint
                     global-eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint)))

  (add-hook 'riot-mode-hook #'riot/use-eslint-from-node-modules)

  (spacemacs/enable-flycheck 'riot-mode))

(defun riot/post-init-js-doc ()
  (add-hook 'riot-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'riot-mode))

(defun riot/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'riot-mode-hook #'smartparens-strict-mode)
    (add-hook 'riot-mode-hook #'smartparens-mode)))

(defun riot/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'riot-mode  "=" 'web-beautify-js))

(defun riot/post-init-web-mode ()
  (define-derived-mode riot-mode web-mode "riot")

  (defun spacemacs//setup-riot-mode ()
    "Adjust web-mode to accommodate riot-mode"
    ;; (emmet-mode 0)
    ;; Enable js-mode snippets
    (yas-activate-extra-mode 'js-mode)
    ;; Don't auto-quote attribute values
    (setq-local web-mode-enable-auto-quoting nil))
  (add-hook 'riot-mode-hook 'spacemacs//setup-riot-mode)

  (add-to-list 'auto-mode-alist '("\\.riot.html\\'" . riot-mode))
  (add-to-list 'auto-mode-alist '("\\.tag\\'" . riot-mode))

  (with-eval-after-load 'riot-mode
   (if (boundp 'web-mode-engines-alist)
       (progn
         (add-to-list 'web-mode-engines-alist '(("riot" . "\\.tag\\'")))
         (add-to-list 'web-mode-engines-alist '(("riot" . "\\.riot.html\\'"))))
     (setq web-mode-engines-alist '(("riot" . "\\.tag\\'")
                                    ("riot" . "\\.riot.html\\'")))))
)
