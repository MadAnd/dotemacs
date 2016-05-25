;;; Compiled snippets and support files for `cc-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cc-mode
                     '(("if" "if (${1:condition}) ${2:{\n    $0\n}}" "if (...) { ... }"
                        (not
                         (eq major-mode 'php-mode))
                        nil nil "/home/madand/.emacs/snippets/cc-mode/if" nil nil)))


;;; Do not edit! File generated at Fri May 20 18:02:05 2016
