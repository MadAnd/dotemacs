;;; config.el --- madand-base layer config file for Spacemacs.
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

(defvar madand-base-font-size-config '()
  "Configuration of the frame font sizes per display widths.

A list of pairs (MIN-WIDTH . FONT-SIZE), where both are integers (pixels).
A pair is considered matching, if the current display width is >= pair's
MIN-WIDTH value. The last matching pair wins - its FONT-SIZE will be used.

The `madand/update-frame-font-size' function implements the described behavior
and can be used with the `window-size-change-functions' hook.

Example: '((0 . 16) (1200 . 15) (1900 . 14)")

(defvar madand-info-mode-text-scale 0.5
  "Text scale factor for `Info-mode'. Will be passed to `text-scale-set'.")

;;; config.el ends here
