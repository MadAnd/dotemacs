(require 'thingatpt)

(defun madand//yas-get-parent-defun-name ()
  (unless (derived-mode-p 'lisp-mode)
    (error "This snippet works only in lisp-mode buffers."))
  (setq temp-buffer (current-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-defun)
      (evil-forward-WORD-begin)
      (thing-at-point 'symbol :no-text-props))))

(defun madand//yas-lisp-infer-package-name ()
  "Infer current package name form file name and path."
  (if (projectile-project-p)
      (thread-last buffer-file-name
        (string-remove-prefix (projectile-project-root))
        (string-remove-prefix "src/")
        (string-remove-suffix ".lisp")
        (concat (projectile-project-name) "/"))
    ""))

(defun madand//following-top-level-defun-name ()
  "Return the name of the first lop-level defun declared after the point.

This actually returns the second item of the nearest top level
form, not only defun."
  (save-match-data
    (save-excursion
      (re-search-forward (rx line-start "("
                             (+ (or (syntax symbol) (syntax word)))
                             (+ (syntax whitespace))
                             (group (+ (or (syntax symbol) (syntax word))))))
      (match-string-no-properties 1))))
