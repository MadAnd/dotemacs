;;; .yas-setup.el --- helper functions for snippets  -*- lexical-binding: t; -*-

;;; Code:

(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(cl-defun madand//yas-project-relative-file-path (&optional (path buffer-file-name))
  "Return current buffer file path relative to the project root.

If a file appears not to belong to any project, return only file
name and file extension, without directory part."
  (if (projectile-project-p)
      (thread-last buffer-file-name
        (string-remove-prefix (projectile-project-root)))
    (format "%s.%s"
            (file-name-base buffer-file-name)
            (file-name-extension buffer-file-name))))

(defun madand//yas-comment-insert-file-path ()
  "Insert current file path as a comment in place WHERE.
Recognized values for WHERE are: top - insert as the first line; bottom - as the
  last line (with \n as a final character); point -
*  "
  (undo-boundary)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (open-line 1)
      (let ((comment-start ";;; "))
        (insert (yas-with-comment (madand//yas-project-relative-file-path))))))
  (message (format "%s" current-prefix-arg)))

(provide '.yas-setup)

;;; .yas-setup.el ends here
