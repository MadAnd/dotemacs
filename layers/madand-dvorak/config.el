;;; config.el --- madand-dvorak layer config file for Spacemacs.
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

(defvar madand-dvorak-digit-keys-alist
  '(("1" . "&")
    ("2" . "[")
    ("3" . "{")
    ("4" . "}")
    ("5" . "(")
    ("6" . "=")
    ("7" . "*")
    ("8" . ")")
    ("9" . "+")
    ("0" . "]"))
  "Alist of digit keys of QWERTY to the corresponding keys of Programmer Dvorak.")

(defvar madand-dvorak-avy-keys '(?h ?t ?n ?s ?u ?e ?o ?a)
  "Keys for performing quick jumps (like avy).")

(defvar madand-dvorak-avy-dispatch-alist
  '((?g . madand//avy-action-translate-stay)
    (?k . avy-action-kill-move)
    (?K . avy-action-kill-stay)
    (?m . avy-action-teleport)
    (?v . avy-action-mark)
    (?c . avy-action-copy)
    (?i . avy-action-ispell)
    (?y . avy-action-yank)
    (?z . avy-action-zap-to-char))
  "Override value of `avy-dispatch-alist' to prevent conflicts when
`avy-keys' is set to Dvorak home keys (aoeuhtns).")

;;; config.el ends here
