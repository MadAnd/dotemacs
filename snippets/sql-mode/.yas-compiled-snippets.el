;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
                     '(("create" "CREATE TABLE ${1:table_name} (\n    ${2:id} ${3:BIGINT} ${4:NOT NULL}\n    $0\n    CONSTRAINT [${5:pk_$1}] PRIMARY KEY ${6:CLUSTERED} ($2)\n);" "create table ..." nil nil nil "/home/madand/.emacs/snippets/sql-mode/create" nil nil)))


;;; Do not edit! File generated at Fri May 20 18:02:05 2016
