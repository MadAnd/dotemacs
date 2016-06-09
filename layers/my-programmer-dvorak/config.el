;;; config.el --- my-programmer-dvorak layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defvar my-programmer-dvorak-digit-keys-alist
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
  "Alist of digit keys of QWERY to the corresponding keys of Programmer Dvorak.")

(defvar my-programmer-dvorak-jump-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
  "List of keys for quick jump packages like `avy' and `ace-window'.

Ensure that these keys not overlap with the ones from
`my-programmer-dvorak-avy-dispatch-alist'.")

(defvar my-programmer-dvorak-avy-dispatch-alist
  '((?k . avy-action-kill-move)
    (?K . avy-action-kill-stay)
    (?m . avy-action-mark)
    (?c . avy-action-copy)
    (?i . avy-action-ispell))
  "Override value of `avy-dispatch-alist' to prevent conflicts  with
`my-programmer-dvorak-jump-keys'.")

;;; config.el ends here
