;;; layers.el --- madand-dvorak layer layers file for Spacemacs.
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

;; Layers required by madand-dvorak layer.

;;; Code:

(configuration-layer/declare-layers '((keyboard-layout
                                       :variables
                                       kl-layout 'dvorak
                                       kl-enabled-configurations '(avy
                                                                   ace-window))))

;;; layers.el ends here
