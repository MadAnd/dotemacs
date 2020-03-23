;;; config.el --- madand-web layer configuration file for Spacemacs.
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

(defvar madand-web-php-fill-column nil
  "Default value of `fill-column' to be used in `php-mode' buffers.
If nil, default value will not be changed.")

(defvar madand-web-format-js-before-save nil
  "Whether to format JS buffers with ‘prettier-js’ before saving.

Use this variable as dir-local, to enable auto-formatting across
entire projects.")
(put 'madand-web-format-js-before-save 'safe-local-variable #'booleanp)

;;; config.el ends here
