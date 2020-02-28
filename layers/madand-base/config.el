;;; config.el --- madand-base layer config file for Spacemacs.
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

;;; Code:

(defvar madand-base-font-size-config '()
  "Configuration of the frame font size for different display widths.

A list of pairs (MIN-WIDTH . FONT-SIZE), where both are integers (in pixels).
A pair is considered matching, if the current display width is >= than pair's
MIN-WIDTH. The last matching pair wins and its FONT-SIZE will be used.

`madand/update-frame-font-size' function implements the described behavior and
can be used with `window-size-change-functions' hook.

Example: '((0 . 16) (1200 . 15) (1900 . 14)")

(defvar madand-doc-modes-text-scale 0.5
  "Text scale factor for documentation modes (e.g. Info, Man).
The value will be passed to `text-scale-set'.")

(defvar madand-irc-text-scale-level 3
  "Text scale level for IRC buffers. See `text-scale-set'.")

(defvar madand-special-modes-re-render-functions '()
  "Information about re-render functions of some special major modes.

The value is alist of (MODE . RE-RENDER-FUNCTION), where MODE designates a
major mode and RE-RENDER-FUNCTION is a function that re-renders its buffer
contents. See `madand//maybe-re-render-special-buffer'.

Example for Man-mode: \(Man-mode . Man-update-manpage\).")

;;; config.el ends here
