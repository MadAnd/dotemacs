;;; config.el --- madand-lisp layer config file for Spacemacs.
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

;; Dumper

(defun madand-lisp/pre-dump ()
  (spacemacs/dump-modes '(lisp-mode sly-mode)))

;;; Code:

(defvar madand-lisp-use-qlot nil
  "Whether to use qlot when starting REPL.

See info node `(elisp)Directory Local Variables'.")
(put 'madand-lisp-use-qlot 'safe-local-variable #'booleanp)

(defvar madand-lisp-src-dir "src/"
  "Path of the source files relative to the project root.")
(put 'madand-lisp-src-dir 'safe-local-variable #'stringp)

(defvar madand-lisp-test-dir "tests/"
  "Path of the test files relative to the project root.")
(put 'madand-lisp-test-dir 'safe-local-variable #'stringp)

(defvar madand-lisp-asdf-path nil
  "Path of the local ASDF installation or nil.

If non-nil, the value is used to set CL_SOURCE_REGISTRY
environment variable. It allows one to have the newer version of
ASDF loaded when starting the REPL. Useful when the ASDF bundled
with the Lisp implementation of choice lacks some needed
features (e.g. support of Package-Local Nicknames).

Here are typical setup steps on a Unix-like OS:
$ mkdir ~/lisp/
$ git clone https://gitlab.common-lisp.net/asdf/asdf.git ~/lisp/asdf
Now set the variable in Emacs:
\(setq madand-lisp-asdf-path \"~/lisp/asdf/\").

NOTE: by default ASDF already recursively scans `~/common-lisp/'
for system definitions. However, setting this variable is useful
when one uses tools (e.g. Qlot) for per-project system isolation,
which make ASDF not to scan default locations.")

;;; config.el ends here
