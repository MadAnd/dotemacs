;;; funcs.el --- madand-lisp layer functions file for Spacemacs. -*- lexical-binding: t; -*-
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

(require 'cl-lib)

;;; ----------------------------------------------------------------------------
;;; Indentation for `cl-flet' and friends
;;; ----------------------------------------------------------------------------

(defvar madand-lisp--elisp-flet-style-macros ()
  "List of Elisp forms that should be indented like `flet'.")

(defun madand/elisp-setup-flet-indent ()
  "Setup proper indentation for `cl-flet' and similar forms in Elisp.
Source: https://emacs.stackexchange.com/a/39177"
  (setq madand-lisp--elisp-flet-style-macros
        (let ((macros '(flet flet* values macrolet labels)))
          (append macros (mapcar (lambda (sym)
                                   (intern (format "cl-%s" (symbol-name sym))))
                                 macros))))
  (dolist (macro madand-lisp--elisp-flet-style-macros)
    (put macro 'lisp-indent-function 'defun))
  (setq lisp-indent-function #'madand/elisp-flet-indent-function))

(defun madand/elisp-flet-indent-function (indent-point state)
  "Handle `flet'-like lists, otherwise call `lisp-indent-function'."
  (cond
   ((ignore-errors
      (save-excursion
        (backward-up-list 3)
        (down-list)
        (when (memq (intern (thing-at-point 'symbol))
                    madand-lisp--elisp-flet-style-macros)
          (forward-sexp 2)
          (>= (point) indent-point))))
    (lisp-indent-defform state indent-point))
   (t (lisp-indent-function indent-point state))))

;;; ----------------------------------------------------------------------------

(defun madand/sly-show-description (string package)
  "Overriding function for `slime-show-description'. It displays description
buffer with `help-mode'."
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (sly-buffer-name :description)))
    (sly-with-popup-buffer (bufname :package package
                                    :connection t
                                    :select sly-description-autofocus
                                    :mode 'help-mode)
      (sly-popup-buffer-mode)
      (princ string)
      (goto-char (point-min)))))

(defun madand/sly-eval-sexp-end-of-line ()
"Evaluate the last sexp at the end of the current line."
(interactive)
(let ((evil-move-beyond-eol t))
  (save-excursion
    (end-of-line)
    (sly-eval-last-expression))))

;;; ----------------------------------------------------------------------------
;;; Projectile support for project type `common-lisp'
;;; ----------------------------------------------------------------------------

(defun madand/project-type-common-lisp-p ()
  "Check whether *.asd files exist in the project root."
  (projectile-verify-file-wildcard "*.asd"))

(defun madand/projectile-find-implementation-or-test--around (f file-name)
  "Around advise for `projectile-find-implementation-or-test'

Call `madand/lisp-projectile-find-implementation-or-test', if the project type
is common-lisp. Otherwise, use the default implementation."
  (if (eq (projectile-project-type) 'common-lisp)
      (madand/lisp-find-implementation-or-test file-name)
    (funcall f file-name)))

(defun madand/lisp-find-implementation-or-test (file-name)
  "Find the implementation/test file for the given FILE-NAME.

This is the replacement of the `projectile-find-implementation-or-test' for
Common Lisp projects."
  (unless file-name (error "The current buffer is not visiting a file"))
  (let* ((project-root (projectile-project-root))
         (relative-path (replace-regexp-in-string
                         (regexp-quote project-root) "" file-name
                         t t))
         (is-test-file (string-prefix-p madand-lisp-test-dir relative-path)))
    (if is-test-file
        (replace-regexp-in-string (regexp-quote madand-lisp-test-dir)
                                  (regexp-quote madand-lisp-src-dir)
                                  file-name t t)
      (replace-regexp-in-string (regexp-quote madand-lisp-src-dir)
                                (regexp-quote madand-lisp-test-dir)
                                file-name t t))))

;;; ----------------------------------------------------------------------------

(defun madand-lisp//turn-off-sly-symbol-completion-mode ()
  "Turn off `sly-symbol-completion-mode'."
  (when (bound-and-true-p sly-symbol-completion-mode)
    (sly-symbol-completion-mode 0)))

(defun madand/sly-connect-to-stumpwm ()
  "Connect to the local StumpWM REPL."
  (interactive)
  (sly-connect "localhost" 4009))

(cl-defun madand//counsel--company-display-transformer--sly (oldfun s)
  "Advice for `counsel--company-display-transformer' compatibility with `sly'."
  (let ((original-result (funcall oldfun s)))
    (if (bound-and-true-p sly-mode)
        (concat (s-pad-right 40 " " s)
                (s-pad-left 20 " " (s-replace s "" original-result)))
      original-result)))

(defun madand/browse-url-eww-new-buffer (url &optional new-window)
  "`browse-url' compatible function to use EWW as a browser."
  (eww url 4))

(defun madand//browse-url-eww--around-advice (fun &rest args)
  "Call FUN in dynamic environment where `browse-url' uses `eww'."
  (let ((browse-url-browser-function #'madand/browse-url-eww-new-buffer))
    (apply fun args)))

;;;-----------------------------------------------------------------------------

(defun madand//evil-fix-point-off-by-one (oldfun &rest args)
  "Compensate for the fact than in evil normal/motion state, point is
actually before the character it is visually displayed on."
  (if (and (or (evil-normal-state-p) (evil-motion-state-p))
           (not (or (eobp) (eolp))))
      (let ((evil-move-beyond-eol t))
        (save-excursion
          (forward-char)
          (apply oldfun args)))
    (apply oldfun args)))

(defun madand/advice-add@evil-fix-point-off-by-one (symbol)
  "Add around advice for SYMBOL with `madand//evil-fix-point-off-by-one'."
  (advice-add symbol :around 'madand//evil-fix-point-off-by-one))

(defun madand/advice-remove@evil-fix-point-off-by-one (symbol)
  "Remove around advice for SYMBOL with `madand//evil-fix-point-off-by-one'."
  (advice-remove symbol 'madand//evil-fix-point-off-by-one))

(defun madand/sly-eval-current-form-sp (&optional arg)
  "Call `sly-eval-last-expression' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'sly-eval-last-expression))))

(defun madand-lisp/clear-default-jump-handlers ()
  "Clear `spacemacs-default-jump-handlers' buffer locally.

This function should be placed in the beginning of the <major-mode>-hook
variable. For this one should use the third argument of `add-hook'.

Example:
\(add-hook 'lisp-mode-hook #'madand/lisp-disable-default-jump-handlers -99)"
  (setq-local spacemacs-default-jump-handlers ()))

(defun madand-lisp/xterm-preserve-colors ()
  "Set `xterm-color-preserve-properties' to t."
  (setq xterm-color-preserve-properties t))

;;; ----------------------------------------------------------------------------
;;; Support for Qlot [https://github.com/fukamachi/qlot]
;;; ----------------------------------------------------------------------------

(defun madand-lisp/project-uses-qlot? ()
  "Return t if the current project uses Qlot.

Check for the presence of a file named \"qlfile\" in the project
root directory. If not in project, return nil."
  (if-let (root (projectile-project-root))
      (file-exists-p (expand-file-name "qlfile" root))))

(defun madand-lisp//sly-start@maybe-use-qlot (oldfun &rest args)
  "Modify args to use `qlot exec' if `madand-lisp-use-qlot' is t.

This is an :filter-args advice for the command `sly-start', which
see."
  (when (and madand-lisp-use-qlot
             (madand-lisp/project-uses-qlot?))
    (cl-destructuring-bind
        (&key program program-args name env &allow-other-keys) args
      (cl-flet ((set-argument (key value)
                  (setq args (plist-put args key value))))
        (set-argument :program "qlot")
        (set-argument :program-args
                      (cl-list* "exec" program
                                ;; Ensure program-args is a list.
                                (cl-etypecase program-args
                                  (list program-args)
                                  (string (list program-args)))))
        (set-argument :directory (projectile-project-root))
        (set-argument :name
                      (intern (format "%s/%s"
                                      (projectile-project-name) name)))
        (set-argument :env
                      (cons (concat "CL_SOURCE_REGISTRY="
                                    (expand-file-name madand-lisp-sly-asdf-path))
                            env)))))
  (apply oldfun args))

;;;-----------------------------------------------------------------------------

(defun madand-lisp/sly-quit-current-lisp ()
  "Quit currently active `sly' Lisp connection, without any prompts.
See `sly-quit-lisp'."
  (interactive)
  (sly-quit-lisp t))

(defun madand-lisp/sly-goto-imports (&optional goto-last)
  "Move point to the first `:import-from' form in the buffer.

If GOTO-LAST is non-nil or when called interactively with the
prefix arg, move point to the last import form."
  (interactive "P")
  (let ((search-string ":import-from")
        result-pos)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (widen)
          (if goto-last
              (progn
                (end-of-buffer)
                (search-backward search-string))
            (beginning-of-buffer)
            (search-forward search-string))
          (setq result-pos (1- (match-beginning 0))))))
    (when result-pos
      (widen)
      (goto-char result-pos))))

;;; ----------------------------------------------------------------------------
;;; Indentation
;;; ----------------------------------------------------------------------------

(defun madand-lisp/indent-buffer ()
  "Indent the whole buffer contents."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun madand-lisp/indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-defun)
    (indent-sexp)))

;;; funcs.el ends here
