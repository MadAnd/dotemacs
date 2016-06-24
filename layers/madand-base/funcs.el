;;; funcs.el --- madand-base layer packages file for Spacemacs.
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

;; General helper functions configuration.

;;; Code:

(defun madand/buffer-dir-name ()
  "Get the name of a directory where the current buffer's file is located. "
  (require 'f)
  (-> (buffer-file-name)
      f-dirname
      f-filename))

(defun madand/browse-url-palemoon (url &optional new-window)
  "Use Palemoon as the external browser."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "palemoon " url) nil
           "palemoon"
           (append
            browse-url-firefox-arguments
            (unless (memq system-type '(windows-nt ms-dos))
              (if (browse-url-maybe-new-window new-window)
                  (if browse-url-firefox-new-window-is-tab
                      '("-new-tab")
                    '("-new-window"))))
            (list url)))))

(defun madand/yas-fallback-to-completing-prompt (orig-fun &rest args)
  "Around advice that temporarily binds `yas-prompt-functions' to
'(yas-completing-prompt)."
  (let ((yas-prompt-functions '(yas/completing-prompt)))
    (apply orig-fun args)))

;;; funcs.el ends here
