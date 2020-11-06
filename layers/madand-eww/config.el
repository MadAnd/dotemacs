;;; config.el --- madand-eww layer configuration file for Spacemacs.
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

(defvar madand-eww-readable-url-regexp-list ()
  "URL regexps for buffers to auto-enable `eww-readable'.

For example
\(add-to-list 'madand-eww-readable-url-regexp-list \"developer\\.mozilla\\.org/\")
enables the readable mode for all MDN pages.")

(defvar madand-eww-no-images-url-regexp-list ()
  "URL regexps for buffers to auto-disable rendering of images.

For example
\(add-to-list 'madand-eww-no-images-url-regexp-list \"HyperSpec/\")
disables image rendering for Common Lisp Hyperspec, regardless of
the domain (works for localhost and file:/// as well).")

;;; config.el ends here
