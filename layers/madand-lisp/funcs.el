;;; funcs.el --- madand-lisp layer functions file for Spacemacs.
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

(defun madand/slime-show-description (string package)
  "Overriding function for `slime-show-description'. It displays description
buffer with `help-mode', instead of default implementation's
`fundamental-mode'."
  (let ((bufname (slime-buffer-name :description)))
    (slime-with-popup-buffer (bufname :package package
                                      :connection t
                                      :select slime-description-autofocus
                                      :mode 'help-mode)
      (princ string)
      (goto-char (point-min)))))

(defun slime-qlot-exec (directory)
  (interactive (list
                (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH="
                                  (mapconcat 'identity exec-path ":"))
                          (concat "QUICKLISP_HOME="
                                  (file-name-as-directory directory) "quicklisp/"))))

;;; funcs.el ends here
