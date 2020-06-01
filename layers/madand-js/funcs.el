;;; funcs.el --- madand-js layer functions file for Spacemacs. -*- lexical-binding: t; -*-
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

;; This is a stripped-down and personalized version of the official javascript
;; layer. Mainly all conditional code for different backends was deleted since
;; LSP and PrettierJS is assumed in my setup.

;;; Code:

(defun madand/nodejs-repl-project-root ()
  "Start NodeJS REPL. If inside a project, change the Node process working dir
to the project root."
  (interactive)
  (nodejs-repl-switch-to-repl)
  (when (projectile-project-p)
    (nodejs-repl--send-string
     (format "process.chdir('%s')\n" (projectile-project-root))))
  (evil-insert-state))

(defun madand/nodejs-repl-send-region-and-focus (beg end)
  "Send region to NodeJS REPL and focus the REPL buffer."
  (interactive "r")
  (nodejs-repl-send-region beg end)
  (nodejs-repl-switch-to-repl))

(defun madand/nodejs-repl-send-buffer-and-focus ()
  "Send buffer to NodeJS REPL and focus the REPL buffer."
  (interactive)
  (nodejs-repl-send-buffer)
  (nodejs-repl-switch-to-repl)
  (evil-insert-state))

(defun madand/nodejs-repl-load ()
  "Load file repl.js form project root, if any."
  (interactive)
  (let ((repl-file (concat (projectile-project-root) "repl.js")))
    (if (file-exists-p repl-file)
        (nodejs-repl-load-file repl-file)
      (nodejs-repl-switch-to-repl))))

(defun madand-js/register-nodejs-repl-bindings ()
  "Register `nodejs-repl' keybindings."
  (interactive)
  (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "nodejs")
  (spacemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "'" 'madand/nodejs-repl-project-root
    "ee" nil
    "eE" nil
    "sb" 'nodejs-repl-send-buffer
    "sB" 'madand/nodejs-repl-send-buffer-and-focus
    "si" 'madand/nodejs-repl-load
    "sf" nil
    "sF" nil
    "sr" 'nodejs-repl-send-region
    "sR" 'madand/nodejs-repl-send-region-and-focus
    "ss" 'madand/nodejs-repl-project-root))

(defun madand/register-skewer-repl-bindings ()
  "Register `nodejs-repl' keybindings."
  (interactive)
  (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "skewer")
  (spacemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "'" 'spacemacs/skewer-start-repl
    "ee" 'skewer-eval-last-expression
    "eE" 'skewer-eval-print-last-expression
    "sb" 'skewer-load-buffer
    "sB" 'spacemacs/skewer-load-buffer-and-focus
    "si" 'spacemacs/skewer-start-repl
    "sf" 'skewer-eval-defun
    "sF" 'spacemacs/skewer-eval-defun-and-focus
    "sr" 'spacemacs/skewer-eval-region
    "sR" 'spacemacs/skewer-eval-region-and-focus
    "ss" 'skewer-repl))

(defvar madand-js-js-repl 'skewer "REPL backend to be used in JavaScript buffers.

Valid values are: nodejs and skewer.")

(defun madand/toggle-skewer-and-nodejs-repl ()
  "Toggle between `skewer-mode' and `nodejs-repl' keybindings."
  (interactive)
  (if (eq 'skewer madand-js-js-repl)
      (progn
        (madand-js/register-nodejs-repl-bindings)
        (setq madand-js-js-repl 'nodejs))
    (madand/register-skewer-repl-bindings)
    (setq madand-js-js-repl 'skewer)))

;;;-----------------------------------------------------------------------------

(defun madand//projectile-test-suffix (current-suffix-function)
  "Custom test suffix function adding support of .test.js files
for nodeapp project type.

Fall back to CURRENT-SUFFIX-FUNCTION for other project types."
  (lambda (project-type)
    (if (member project-type '(madand-nodeapp))
        ".test"
      (funcall current-suffix-function project-type))))

(defun madand-js/set-js-buffer-dash-docsets ()
  "Set `counsel-dash-docsets' for JavaScript buffers."
  (setq-local counsel-dash-docsets '("JavaScript" "VueJS" "jQuery" "Lo-Dash")
              counsel-dash-common-docsets '()))

(defun madand-js/maybe-turn-on-prettier-js ()
  "Conditionally turn on `prettier-js-mode'.

`prettier-js-mode' will be turned on only if the value of a
variable `madand-js-format-code-before-save' is t."
  (when madand-js-format-code-before-save
    (prettier-js-mode)))

(defun madand-js/insert-comma-before-eol-and-move ()
  "Insert comma after the last character of the current line and move point."
  (interactive)
  (end-of-line)
  (insert ","))

(defun madand-js//setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-firefox)
  (require 'dap-chrome))

(defun madand-js//js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js2-doc' for the given MODE."
  (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
  (spacemacs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))

;;;-----------------------------------------------------------------------------

;;;  nodejs-repl integration

(defun madand-js/nodejs-repl-project-root ()
  "Start NodeJS REPL with project root as current directory.

If inside a (projectile) project, change the Node process working
directory to the project root."
  (interactive)
  (nodejs-repl-switch-to-repl)
  (when (projectile-project-p)
    (nodejs-repl--send-string
     (format "process.chdir('%s')\n" (projectile-project-root))))
  (evil-insert-state))

(defun madand-js/nodejs-repl-load ()
  "Load the repl.js file in project root, if exists.
If the file not exists, just switch to the REPL."
  (interactive)
  (let ((repl-file (concat (projectile-project-root) "repl.js")))
    (if (file-exists-p repl-file)
        (nodejs-repl-load-file repl-file)
      (nodejs-repl-switch-to-repl))))

(defun madand-js/nodejs-send-last-expression-and-focus ()
  "Evaluate last expression in the REPL and focus its window."
  (interactive)
  (nodejs-repl-send-last-expression)
  (nodejs-repl-switch-to-repl))

(defun madand-js/nodejs-send-buffer-and-focus ()
  "Evaluate last expression in the REPL and focus its window."
  (interactive)
  (nodejs-repl-send-buffer)
  (nodejs-repl-switch-to-repl))

(defun madand-js/nodejs-send-line-and-focus ()
  "Evaluate last expression in the REPL and focus its window."
  (interactive)
  (nodejs-repl-send-line)
  (nodejs-repl-switch-to-repl))

(defun madand-js/nodejs-send-region-and-focus (beg end)
  "Evaluate last expression in the REPL and focus its window."
  (interactive "r")
  (nodejs-repl-send-region start end)
  (nodejs-repl-switch-to-repl))

;;;-----------------------------------------------------------------------------

;;; skewer

(defun madand-js/skewer-start-repl ()
  "Attach a browser to Emacs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun madand-js/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun madand-js/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun madand-js/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun madand-js/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (madand-js/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))

;;; funcs.el ends here
