;;; funcs.el --- madand-ivy layer functions file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; General helper functions configuration.

;;; Code:

(defun madand-ivy//ivy-occur-press@reposition ()
  "Call `reposition-window' in the last ivy-occur target buffer."
  (with-selected-window (ivy-state-window ivy-occur-last)
    (reposition-window)))

;;; funcs.el ends here
