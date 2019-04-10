;;; packages.el --- madand-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2018 Andriy Kmit'
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst madand-lisp-packages
  '((elisp-mode :location built-in)
    (evil-cleverparens :excluded t)
    lispy
    projectile
    (common-lisp-snippets :requires yasnippet)
    xterm-color
    popwin
    helm
    evil
    parinfer
    company
    smartparens
    (sly :requires smartparens)
    (sly-mrepl :requires sly :location built-in)
    (sly-macrostep :requires (sly macrostep))
    (sly-repl-ansi-color :requires sly)))

(defun madand-lisp/init-elisp-mode ()
  ;; Setup proper indentation for `cl-flet' and friends.
  ;; https://emacs.stackexchange.com/a/39177
  (defvar elisp-flet-style-macros
    (let ((macs '(flet flet* values macrolet labels)))
      (append macs (mapcar (lambda (sym)
                             (intern (format "cl-%s" (symbol-name sym))))
                           macs))))
  (dolist (mac elisp-flet-style-macros)
    (put mac 'lisp-indent-function 'defun))
  (setq lisp-indent-function #'madand/elisp-flet-indent-function))

(defun madand-lisp/init-lispy ()
  (use-package lispy
    :init
    (progn
      (let ((mode-hooks '(emacs-lisp-mode-hook
                          lisp-mode-hook)))
        (spacemacs/add-to-hooks #'lispy-mode mode-hooks)
        ;; Disable Smartparens altogether since Lispy already does the same.
        (when (configuration-layer/package-usedp 'smartparens)
          (spacemacs/add-to-hooks #'turn-off-smartparens-mode mode-hooks))))
    :config
    (progn
      )))

(defun madand-lisp/post-init-projectile ()
  (with-eval-after-load 'projectile
    (advice-add 'projectile-find-implementation-or-test :around
                #'madand/projectile-find-implementation-or-test--around)
    (projectile-register-project-type
     'common-lisp #'madand/project-type-common-lisp-p
     :test-dir "test/")))

(defun madand-lisp/pre-init-xterm-color ()
  (when (configuration-layer/package-usedp 'sly)
    (add-hook 'sly-mrepl-mode-hook (lambda () (setq xterm-color-preserve-properties t)))))

(defun madand-lisp/pre-init-popwin ()
  (spacemacs|use-package-add-hook sly
    :post-config
    (push '("*sly-description*" :width 0.5 :position right)
          popwin:special-display-config)
    (push '("*sly-macroexpansion*" :width 0.5 :position right)
          popwin:special-display-config)))

(defun madand-lisp/init-common-lisp-snippets ())

(defun madand-lisp/pre-init-helm ()
  (spacemacs|use-package-add-hook sly
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "sI" 'spacemacs/helm-sly)))

(defun madand-lisp/post-init-parinfer ()
  (add-hook 'lisp-mode-hook 'parinfer-mode))

(defun madand-lisp/pre-init-evil ()
  (with-eval-after-load 'evil
    (when (configuration-layer/package-used-p 'sly)
      (evil-set-initial-state 'sly-mrepl-mode 'insert)
      (evil-set-initial-state 'sly-inspector-mode 'emacs)
      (evil-set-initial-state 'sly-db-mode 'emacs)
      (evil-set-initial-state 'sly-xref-mode 'emacs)
      (evil-set-initial-state 'sly-stickers--replay-mode 'emacs))))

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
      (spacemacs/register-repl 'sly 'sly)
      (setq sly-autodoc-use-multiline t
            sly-complete-symbol*-fancy t
            sly-kill-without-query-p t
            sly-repl-history-remove-duplicates t
            sly-repl-history-trim-whitespaces t
            sly-net-coding-system 'utf-8-unix)
      (sly-setup '(sly-fancy))
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "'" #'sly
        "ha" #'sly-apropos
        "hb" #'sly-who-binds
        "hd" #'sly-disassemble-symbol
        "hh" #'sly-describe-symbol
        "hH" #'sly-hyperspec-lookup
        "hm" #'sly-who-macroexpands
        "hp" #'sly-apropos-package
        "hr" #'sly-who-references
        "hs" #'sly-who-specializes
        "hS" #'sly-who-sets
        "h<" #'sly-who-calls
        "h>" #'sly-calls-who
        "cc" #'sly-compile-file
        "cC" #'sly-compile-and-load-file
        "cf" #'sly-compile-defun
        "cl" #'sly-load-file
        "cn" #'sly-remove-notes
        "cr" #'sly-compile-region
        "eb" #'sly-eval-buffer
        "ee" #'sly-eval-last-expression
        "eE" #'sly-eval-print-last-expression
        "ef" #'sly-eval-defun
        "eF" #'slime-undefine-function
        "el" #'madand/sly-eval-sexp-end-of-line
        "er" #'sly-eval-region
        "gg" #'spacemacs/common-lisp-navigation-transient-state/body
        "me" #'sly-macroexpand-1
        "mE" #'sly-macroexpand-all
        "sc" #'sly-mrepl-clear-repl
        "si" #'sly
        "sq" #'sly-quit-lisp
        "sr" #'sly-restart-inferior-lisp
        "ss" #'sly-mrepl-sync
        "Sb" #'sly-stickers-toggle-break-on-stickers
        "Sc" #'sly-stickers-clear-defun-stickers
        "SC" #'sly-stickers-clear-buffer-stickers
        "Sf" #'sly-stickers-fetch
        "Sr" #'sly-stickers-replay
        "Ss" #'sly-stickers-dwim
        "tt" #'sly-toggle-trace-fdefinition
        "tT" #'sly-toggle-fancy-trace
        "tu" #'sly-untrace-all)
      (mapc (lambda (x)
              (spacemacs/declare-prefix-for-mode 'lisp-mode (car x) (cdr x)))
            '(("mc" . "compile")
              ("me" . "evaluate")
              ("mg" . "navigation")
              ("mh" . "help")
              ("mm" . "macro")
              ("ms" . "repl")
              ("mS" . "stickers")
              ("mt" . "trace")))
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
      (spacemacs|define-jump-handlers lisp-mode )
      (evil-define-key 'normal sly-popup-buffer-mode-map "Q" #'kill-buffer-and-window)
      (add-hook 'sly-popup-buffer-mode-hook #'spacemacs/toggle-rainbow-identifier-off)
      (advice-add 'sly-show-description
                  :override #'madand/sly-show-description)
      (setq sly-lisp-implementations '((roswell ("/usr/bin/ros" "-Q" "run"))
                                       (sbcl ("/usr/bin/ros")))
            sly-default-lisp 'roswell))))

(defun madand-lisp/init-sly-mrepl ()
  (use-package sly-mrepl
    :after sly
    :bind
    (:map sly-mrepl-mode-map
          ("<up>" . sly-mrepl-previous-input-or-button)
          ("<down>" . sly-mrepl-next-input-or-button)
          ("<C-up>" . sly-mrepl-previous-input-or-button)
          ("<C-down>" . sly-mrepl-next-input-or-button))))

(defun madand-lisp/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes sly-mode sly-mrepl-mode))

(defun madand-lisp/init-sly-macrostep ()
  (use-package sly-macrostep
    :after sly
    :config
    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "ms" 'spacemacs/macrostep-transient-state/body))))

(defun madand-lisp/init-sly-repl-ansi-color ()
  (use-package sly-repl-ansi-color
    :demand t
    :config (push 'sly-repl-ansi-color sly-contribs)))

;;; packages.el ends here
