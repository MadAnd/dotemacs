;;; packages.el --- madand-eww layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;; Based on: https://github.com/coljamkop/eww-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;; ----------------------------------------------------------------------------
;;; Jump between eww buffers
;;; ----------------------------------------------------------------------------

(defvar madand--eww-buffers nil)

(defun madand-eww//get-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'eww-mode)
                 (not (memq buffer madand--eww-buffers)))
        (push buffer madand--eww-buffers))))
  (unless madand--eww-buffers
    (error "No eww buffers"))
  ;; remove deleted buffers maintaining order
  (dolist (buffer madand--eww-buffers)
    (if (not (memq buffer (buffer-list)))
        (delq buffer madand--eww-buffers)))
  madand--eww-buffers)

(defun madand-eww//next-buffer (buff)
  (let* ((eww-buffers (madand-eww//get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (eq eww-buffer-pos (1- (length eww-buffers)))
        (car eww-buffers)
      (nth (1+ eww-buffer-pos) eww-buffers))))

(defun madand-eww//previous-buffer (buff)
  (let* ((eww-buffers (madand-eww//get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (zerop eww-buffer-pos)
        (car (last eww-buffers))
      (nth (1- eww-buffer-pos) eww-buffers))))

(defun madand/eww-jump-next-buffer ()
  (interactive)
  (pop-to-buffer-same-window (madand-eww//next-buffer (current-buffer))))

(defun madand/eww-jump-previous-buffer ()
  (interactive)
  (pop-to-buffer-same-window (madand-eww//previous-buffer (current-buffer))))

;;; ----------------------------------------------------------------------------
;;; Auto-highlighting of code snippets inside of <pre>-tags
;;; ----------------------------------------------------------------------------

;; https://github.com/andreasjansson/language-detection.el

(defun madand-eww//tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (madand-eww//fontify-pre dom))
    (shr-ensure-newline)))

(defun madand-eww//fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (madand-eww//buffer-auto-detect-mode)))
      (when mode
        (madand-eww//fontify-buffer mode)))
    (buffer-string)))

(defun madand-eww//fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun madand-eww//buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (when (fboundp mode)
      mode)))

;;; ----------------------------------------------------------------------------
;;; Misc functions
;;; ----------------------------------------------------------------------------

(defun madand-eww//match-any-regexp? (str regexp-list)
  "Return non-nil if STR matches any regexp in REGEXP-LIST."
  (cl-some (lambda (regexp) (string-match-p regexp str)) regexp-list))

(defvar madand-eww--recursive-reload? nil
  "Recursion guard for ‘madand-eww//maybe-turn-on-extra-visual-modes’.")

(cl-defun madand-eww//maybe-turn-on-extra-visual-modes ()
  "Turn on extra visual modes if the URL matches a corresponding pattern.

See:
* ‘madand-eww-readable-url-regexp-list’
* ‘eww-after-render-hook’"
  (when madand-eww--recursive-reload?
    (setq madand-eww--recursive-reload? nil)
    (cl-return-from madand-eww//maybe-turn-on-extra-visual-modes))
  (let ((url (plist-get eww-data :url))
        (need-reload? nil))
    (cl-macrolet ((maybe-turn-on (pre-cond regexp-list &body body)
                    `(when (and ,pre-cond
                                (madand-eww//match-any-regexp? url ,regexp-list))
                       ,@body
                       (setq need-reload? t))))
      (maybe-turn-on t
                     madand-eww-readable-url-regexp-list
                     (eww-readable)))
    (when need-reload?
      (setq madand-eww--recursive-reload? t)
      (eww-reload))))

(cl-defun madand-eww//ace-link--eww-action@fix-off-by-one ((pt))
  "Fix off-by-one point preventing jumps on single letter links."
  (when (number-or-marker-p pt)
    (setq pt (1- pt)))
  (list pt))

;;; funcs.el ends here
