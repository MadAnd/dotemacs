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

(defun madand//eww-get-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'eww-mode)
                 (not (memq buffer madand--eww-buffers)))
        (push buffer
              madand--eww-buffers))))
  (unless madand--eww-buffers
    (error "No eww buffers"))
  ;; remove deleted buffers maintaining order
  (dolist (buffer madand--eww-buffers)
    (if (not (memq buffer (buffer-list)))
        (delq buffer madand--eww-buffers)))
  madand--eww-buffers)

(defun madand//eww-next-buffer (buff)
  (let* ((eww-buffers (madand//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (eq eww-buffer-pos (1- (length eww-buffers)))
        (car eww-buffers)
      (nth (1+ eww-buffer-pos) eww-buffers))))

(defun madand//eww-previous-buffer (buff)
  (let* ((eww-buffers (madand//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (zerop eww-buffer-pos)
        (car (last eww-buffers))
      (nth (1- eww-buffer-pos) eww-buffers))))

(defun madand/eww-jump-next-buffer ()
  (interactive)
  (pop-to-buffer-same-window (madand//eww-next-buffer (current-buffer))))

(defun madand/eww-jump-previous-buffer ()
  (interactive)
  (pop-to-buffer-same-window (madand//eww-previous-buffer (current-buffer))))

;;; ----------------------------------------------------------------------------
;;; Auto-highlighting of code snippets inside of <pre>-tags
;;; ----------------------------------------------------------------------------

;; https://github.com/andreasjansson/language-detection.el

(defun madand//eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (madand//eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun madand//eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (madand//eww-buffer-auto-detect-mode)))
      (when mode
        (madand//eww-fontify-buffer mode)))
    (buffer-string)))

(defun madand//eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun madand//eww-buffer-auto-detect-mode ()
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

(defun madand//eww-turn-on-word-wrapping ()
  "Turn on word wrapping: disable `truncate-lines' and enable `word-wrap'."
  (setq word-wrap t
        truncate-lines nil)
  (force-mode-line-update))

(defun madand//eww-maybe-turn-on-centered-buffer-mode ()
  "Turn on `writeroom-mode' if URL matches a pattern.
See `madand-eww-centered-buffer-url-patterns', `eww-after-render-hook'."
  (let ((url (plist-get eww-data :url)))
    (cl-some (lambda (pattern)
               (when (and (not writeroom-mode)
                          (string-match-p pattern url))
                 (spacemacs/toggle-centered-buffer)
                 (eww-reload)))
             madand-eww-centered-buffer-url-patterns)))

;;; funcs.el ends here
