;;; funcs.el --- madand-ivy layer functions file for Spacemacs.
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

;; General helper functions configuration.

;;; Code:

(defun madand/ivy-rich-switch-buffer-pad (str len &optional left)
  "Improved version of `ivy-rich-switch-buffer-pad' that truncates long inputs."
  (let ((real-len (length str)))
    (cond
      ((< real-len len) (if left
                            (concat (make-string (- len real-len) ? ) str)
                          (concat str (make-string (- len real-len) ? ))))
      ((= len real-len) str)
      (t (concat (substring str 0 (- len 1)) "…")))))

;;; funcs.el ends here
