;;; layers.el --- madand-web layer layers file for Spacemacs.
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

;; Layers required by madand-web layer.

;;; Code:

(configuration-layer/declare-layers '(dap
                                      (javascript :variables
                                                  javascript-backend 'lsp
                                                  javascript-fmt-tool 'prettier
                                                  import-js 'import-js
                                                  javascript-repl 'skewer)
                                      html
                                      php
                                      (typescript :variables
                                                  typescript-fmt-tool 'prettier
                                                  typescript-backend 'lsp)))

;;; layers.el ends here
