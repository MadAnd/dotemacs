;;; Compiled snippets and support files for `prog-mode'
;;; contents of the .yas-setup.el support file:
;;;
(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))
;;; Snippet definitions:
;;;
(yas-define-snippets 'prog-mode
                     '(("t" "`(yas-with-comment \"TODO: \")`" "todo"
                        (not
                         (eq major-mode 'php-mode))
                        nil nil "/home/madand/.emacs/snippets/prog-mode/todo" nil nil)))


;;; Do not edit! File generated at Fri May 20 18:02:05 2016
