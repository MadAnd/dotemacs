;;; layers.el --- madand-ivy layer layers file for Spacemacs.
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

;; Layers required by madand-ivy layer.

;;; Code:

(configuration-layer/declare-layers '((ivy :variables
                                           ivy-use-virtual-buffers nil)))

;;; layers.el ends here
