;;; packages.el --- madand-lisp layer packages file for Spacemacs.
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

(defconst madand-lisp-packages
  '(
    ace-link
    auto-highlight-symbol
    (elisp-mode :location built-in)
    (evil-cleverparens :excluded t)
    (lisp-mode :location built-in)
    lispy
    lispyville
    projectile
    evil
    company
    smartparens
    (sly :requires smartparens)
    (sly-macrostep :requires (sly macrostep))
    (sly-repl-ansi-color :requires sly)
    xterm-color
    ))

(defun madand-lisp/post-init-ace-link ()
  (with-eval-after-load 'sly
    (dolist (map '(sly-trace-dialog-mode))
      (evil-define-key 'evilified map "o" #'ace-link-help))))

(defun madand-lisp/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'lisp-mode)))

(defun madand-lisp/init-elisp-mode ()
  (add-hook 'emacs-lisp-mode-hook #'madand-lisp/clear-default-jump-handlers -99)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  (with-eval-after-load 'elisp-mode
    (madand/elisp-setup-flet-indent)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      "hh" 'helpful-at-point)))

(defun madand-lisp/init-lisp-mode ()
  (add-hook 'lisp-mode-hook #'madand-lisp/clear-default-jump-handlers -99)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (add-hook 'lisp-mode-hook #'lispyville-mode))

(defun madand-lisp/init-lispy ()
  (use-package lispy
    :defer t))

(defun madand-lisp/init-lispyville ()
  (use-package lispyville
    :defer t))

(defun madand-lisp/post-init-projectile ()
  (with-eval-after-load 'projectile
    (advice-add 'projectile-find-implementation-or-test :around
                #'madand/projectile-find-implementation-or-test--around)
    (projectile-register-project-type
     'common-lisp #'madand/project-type-common-lisp-p
     :test-dir "tests/")))

(defun madand-lisp/pre-init-evil ()
  (when (configuration-layer/package-used-p 'sly)
    (madand/advice-add@evil-fix-point-off-by-one 'sly-last-expression)))

(defun madand-lisp/pre-init-smartparens ()
  (with-eval-after-load 'smartparens
    (when (configuration-layer/package-used-p 'sly)
      (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
      (sp-local-pair '(sly-mrepl-mode) "`" "`" :actions nil))))

(defun madand-lisp/init-sly ()
  (use-package sly
    :defer t
    :init
    (progn
      (setq sly-net-coding-system 'utf-8-unix)
      (spacemacs/register-repl 'sly 'sly)
      ;; Setup initial evil states for various sly modes.
      (evil-set-initial-state 'sly-mrepl-mode 'normal)
      (dolist (mode '(sly-db-mode
                      sly-inspector-mode
                      sly-trace-dialog-mode
                      sly-stickers--replay-mode
                      sly-xref-mode))
        (evil-set-initial-state mode 'motion))
      (dolist (contrib '(sly-scratch))
        (add-to-list 'sly-contribs contrib))
      ;; Hyperspec setup.
      (setq common-lisp-hyperspec-root
            (concat "file://" (expand-file-name "~/docs/CommonLisp/HyperSpec/")))
      (dolist (fun '(common-lisp-hyperspec
                     common-lisp-hyperspec-lookup-reader-macro
                     common-lisp-hyperspec-format
                     common-lisp-hyperspec-glossary-term))
        (advice-add fun :around #'madand//browse-url-eww--around-advice))
      (spacemacs|define-jump-handlers lisp-mode sly-edit-definition)
      (spacemacs|define-jump-handlers sly-mrepl-mode
                                      sly-edit-definition-other-window)

      (dolist (x '(("m=" . "format")
                   ("mh" . "help")
                   ("mc" . "compile")
                   ("me" . "evaluate")
                   ("mE" . "errors/notes")
                   ("mg" . "navigation")
                   ("mi" . "import/export")
                   ("mm" . "macro")
                   ("ms" . "repl")
                   ("mS" . "stickers")
                   ("mt" . "trace")))
        (spacemacs/declare-prefix-for-mode 'sly-mode (car x) (cdr x)))
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "'"  'sly-mrepl
        ;; "E"  'sly-edit-value
        ;; Code Formatting
        "==" 'indent-sexp
        "=b" 'madand-lisp/indent-buffer
        "=f" 'madand-lisp/indent-defun
        ;; Help
        "ha" 'sly-apropos
        "hA" 'sly-apropos-all
        "hb" 'sly-who-binds
        "hd" 'sly-disassemble-symbol
        "hF" 'hyperspec-lookup-format
        "hh" 'sly-describe-symbol
        "hH" 'sly-hyperspec-lookup
        "hm" 'sly-who-macroexpands
        "hp" 'sly-apropos-package
        "hr" 'sly-who-references
        "hR" 'hyperspec-lookup-reader-macro
        "hs" 'sly-who-specializes
        "hS" 'sly-who-sets
        "h<" 'sly-who-calls
        "h>" 'sly-calls-who
        ;; Compilation
        "cc" 'sly-compile-file
        "cC" 'sly-compile-and-load-file
        "cf" 'sly-compile-defun
        "cl" 'sly-load-file
        "cr" 'sly-compile-region
        ;; Evaluation
        "eb" 'sly-eval-buffer
        "ec" 'madand/sly-eval-current-form-sp
        "ee" 'sly-eval-last-expression
        "eE" 'sly-eval-print-last-expression
        "ef" 'sly-eval-defun
        "eF" 'sly-undefine-function
        "eI" 'sly-unintern-symbol
        "el" 'madand/sly-eval-sexp-end-of-line
        "er" 'sly-eval-region
        "eP" 'sly-pprint-eval-last-expression
        ;; Errors/notes
        "En" 'sly-next-note
        "EN" 'sly-previous-note
        "Er" 'sly-remove-notes
        ;; Navigation
        "g." 'spacemacs/common-lisp-navigation-transient-state/body
        "gb" 'sly-pop-find-definition-stack
        "gi" 'madand-lisp/sly-goto-imports
        "gu" 'sly-edit-uses
        ;; Import/export
        "ii" 'sly-import-symbol-at-point
        "ie" 'sly-export-symbol-at-point
        "ic" 'sly-export-class
        ;; Macroexpand
        "me" 'sly-macroexpand-1
        "mE" 'sly-macroexpand-all
        ;; REPL
        "sc" 'sly-mrepl-clear-repl
        "si" 'sly
        "sI" 'sly-interrupt
        "sl" 'sly-list-connections
        "sN" 'sly-mrepl-new
        "sn" 'sly-next-connection
        "sp" 'sly-prev-connection
        "sq" 'sly-quit-lisp
        "sQ" 'madand-lisp/sly-quit-current-lisp
        "sr" 'sly-restart-inferior-lisp
        "ss" 'sly-mrepl-sync
        "sw" 'madand/sly-connect-to-stumpwm
        ;; Stickers
        "Sb" 'sly-stickers-toggle-break-on-stickers
        "Sc" 'sly-stickers-clear-defun-stickers
        "SC" 'sly-stickers-clear-buffer-stickers
        "Sf" 'sly-stickers-fetch
        "Sr" 'sly-stickers-replay
        "Ss" 'sly-stickers-dwim
        ;; trace
        "td" 'sly-trace-dialog
        "tt" 'sly-trace-dialog-toggle-trace
        "tT" 'sly-toggle-fancy-trace
        "tu" 'sly-untrace-all)
      (spacemacs|define-transient-state common-lisp-navigation
        :title "Common Lisp Navigation Transient State"
        :doc "
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition

[_q_] Exit"
        :foreign-keys run
        :bindings
        ("g" sly-edit-definition)
        ("G" sly-edit-definition-other-window)
        ("b" sly-pop-find-definition-stack)
        ("n" sly-next-note)
        ("N" sly-previous-note)
        ("s" sly-stickers-next-sticker)
        ("S" sly-stickers-prev-sticker)
        ("q" nil :exit t)))
    :config
    (progn
      (setq sly-lisp-implementations
            '((ros-sbcl ("ros" "-L" "sbcl-bin" "run"))
              (ros-ecl ("ros" "-L" "ecl" "run")))
            sly-default-lisp 'ros-sbcl
            sly-description-autofocus t
            sly-command-switch-to-existing-lisp 'never
            sly-auto-select-connection 'always
            sly-kill-without-query-p t)
      (add-hook 'sly-popup-buffer-mode-hook
                #'spacemacs/toggle-rainbow-identifier-off)
      (add-hook 'sly-mode-hook #'madand-lisp//turn-off-sly-symbol-completion-mode)
      (add-hook 'sly-mrepl-mode-hook #'yas-minor-mode)
      (define-key sly-xref-mode-map (kbd "j") #'sly-xref-next-line)
      (define-key sly-xref-mode-map (kbd "k") #'sly-xref-prev-line)
      (evil-define-key 'normal sly-popup-buffer-mode-map
        "Q" #'kill-buffer-and-window)
      (advice-add 'sly-show-description :override #'madand/sly-show-description)
      (advice-add 'sly-start :around #'madand-lisp//sly-start@maybe-use-qlot)
      )))

(defun madand-lisp/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes sly-mode sly-mrepl-mode)
  (with-eval-after-load 'counsel
    (advice-add 'counsel--company-display-transformer :around
                #'madand//counsel--company-display-transformer--sly)))

(defun madand-lisp/init-sly-macrostep ()
  (use-package sly-macrostep
    :after sly
    :config
    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "ms" 'spacemacs/macrostep-transient-state/body))))

(defun madand-lisp/init-sly-repl-ansi-color ()
  (cl-pushnew 'sly-repl-ansi-color sly-contribs)
  (use-package sly-repl-ansi-color
    :defer t))

(defun madand-lisp/pre-init-xterm-color ()
  (when (configuration-layer/package-usedp 'sly)
    (add-hook 'sly-mrepl-mode-hook #'madand-lisp/xterm-preserve-colors)))

;;; packages.el ends here
