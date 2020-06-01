
(defvar madand-js-format-code-before-save t
  "If t, turn on `prettier-js-mode' in a buffer.

You can set this variable in .dir-locals.el to for more granular
control over the automatic formatting.")
(put 'madand-js-format-code-before-save 'safe-local-variable #'booleanp)
