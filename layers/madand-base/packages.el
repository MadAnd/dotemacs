;;; packages.el --- madand-base layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Base layer with general configuration.

;;; Code:

(defconst madand-base-packages
  '(
    ace-jump-helm-line
    ace-popup-menu
    (browse-url :location built-in)
    cider
    clojure-mode
    ;; clojure-cheatsheet
    (emacs-lisp :location built-in)
    company
    editorconfig
    flyspell
    evil
    evil-evilified-state
    evil-magit
    evil-mc
    helm
    magit
    persp-mode
    (python :location built-in)
    (simple :location built-in)
    simple-mpc
    semantic
    (smerge-mode :location built-in)
    yasnippet))


(defun madand-base/post-init-ace-jump-helm-line ()
  (setq ace-jump-helm-line-style 'pre)

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-'") #'ace-jump-helm-line-and-select)))

(defun madand-base/init-ace-popup-menu ()
  (spacemacs/defer-until-after-user-config
   (lambda ()
     (ace-popup-menu-mode 1))))

(defun madand-base/init-browse-url ()
  (setq browse-url-browser-function #'madand/browse-url-palemoon))

(defun madand-base/post-init-cider ()
  (with-eval-after-load 'cider
    (setq cider-repl-display-help-banner nil)))

(defun madand-base/post-init-clojure-mode ()
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  )

(defun madand-base/post-init-emacs-lisp ()
  ;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  )

(defun madand-base/post-init-company ()
  (with-eval-after-load 'company
    (add-hook 'company-completion-started-hook 'madand//company-turn-off-page-break-lines)
    (add-hook 'company-completion-finished-hook 'madand//company-maybe-turn-on-page-break-lines)
    (add-hook 'company-completion-cancelled-hook 'madand//company-maybe-turn-on-page-break-lines)))

(defun madand-base/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init
    (add-hook 'prog-mode-hook #'editorconfig-mode)
    :config
    (spacemacs|hide-lighter editorconfig-mode)))

(defun madand-base/post-init-evil ()
  (define-key evil-normal-state-map (kbd "gp") #'madand/evil-select-pasted))

(defun madand-base/post-init-evil-evilified-state ()
  (define-key evil-evilified-state-map-original (kbd "Q") 'spacemacs/kill-this-buffer))

(defun madand-base/post-init-evil-magit ()
  (with-eval-after-load 'evil-magit
    (evil-magit-define-key 'normal 'magit-mode-map "yB" 'madand/magit-copy-section-value-short)))

(defun madand-base/init-info ()
  (setq Info-additional-directory-list (list (expand-file-name "~/.spacemacs.d/info/"))))

(defun madand-base/post-init-evil-mc ()
  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode))

(defun madand-base/post-init-flyspell ()
  (with-eval-after-load 'flyspell
    (define-key evil-normal-state-map (kbd "zi") 'flyspell-correct-word-before-point)))

(defun madand-base/post-init-helm ()
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-map (kbd "C-u") #'helm-find-files-up-one-level)
    (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' \
            --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'"
                                           "--colors 'match:bg:yellow'"))))

(defun madand-base/post-init-magit ()
  (with-eval-after-load 'magit
    (setq magit-diff-refine-hunk 'all
          magit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ai
Commit:     %cN <%cE>
CommitDate: %ci\n")
    (evil-magit-define-key 'normal 'magit-revision-mode-map (kbd "yd") #'madand/magit-copy-commit-date)
    (add-hook 'magit-status-headers-hook 'magit-insert-repo-header t)))

(defun madand-base/post-init-magit ()
  (spacemacs/defer-until-after-user-config
   (lambda ()
     (ace-popup-menu-mode 1))))

(defun madand-base/post-init-persp-mode ()
  (with-eval-after-load 'persp-mode
    (setq persp-auto-save-opt 0)))

(defun madand-base/post-init-python ()
  (with-eval-after-load 'python
    (add-hook 'python-mode-hook (lambda ()
                                  (setq-local zeal-at-point-docset "python")))))

(defun madand-base/init-simple ()
  (with-eval-after-load 'simple
    (add-hook 'find-file-hook #'madand-base/maybe-disable-fly-modes)))

(defun madand-base/init-simple-mpc ()
  (use-package simple-mpc
    :commands simple-mpc-view-current-playlist
    :init
    (spacemacs/set-leader-keys "am" #'simple-mpc-view-current-playlist)
    :config
    (progn
      (spacemacs|hide-lighter simple-mpc-current-playlist-mode)
      (setq simple-mpc-mpd-playlist-directory "~/.config/mpd/playlists/"
            simple-mpc-playlist-auto-refresh 2)
      (evilified-state-evilify-map simple-mpc-mode-map
        :mode simple-mpc-mode
        :bindings (kbd "C-m") (kbd "<return>")))))

(defun madand-base/post-init-semantic ()
  (with-eval-after-load 'semantic
    ;; Workaround for semantic freezing emacs when typing text in elisp strings.
    (add-hook
     'semantic-mode-hook
     (lambda ()
       (when (eq major-mode 'emacs-lisp-mode)
         (dolist (x '(semantic-analyze-completion-at-point-function
                      semantic-analyze-notc-completion-at-point-function
                      semantic-analyze-nolongprefix-completion-at-point-function))
           (remove-hook 'completion-at-point-functions x)))))))

(defun madand-base/post-init-smerge-mode ()
  (with-eval-after-load 'smerge-mode
    (add-hook 'smerge-mode-hook #'madand//smerge-disable-rainbow-identifiers)))

(defun madand-base/post-init-yasnippet ()
  ;; Force use of `ace-popup-menu'.
  (setq yas-prompt-functions '(yas-x-prompt))
  ;; Fall back to completing prompt for yasnippet functions with
  ;; too many choices to be shown via `ace-popup-menu'.
  (advice-add 'yas-visit-snippet-file :around
              #'madand/yas-fallback-to-completing-prompt)
  (advice-add 'yas-insert-snippet :around
              #'madand/yas-fallback-to-completing-prompt)

  (with-eval-after-load 'yasnippet
    ;; Expand snippets with SPC.
    (evil-define-key 'hybrid yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
    ;; (evil-define-key 'hybrid yas-minor-mode-map (kbd "SPC")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (unless (or (= (point) (point-min))
    ;;                 (= (point) (point-max))
    ;;                 (seq-contains [?\s ?\t ?\r ?\n ?-] (char-before))
    ;;                 (not (seq-contains [?\s ?\t ?\r ?\n] (char-after))))
    ;;       (call-interactively #'madand/cameldash-word-at-point))
    ;;     (call-interactively #'yas-expand)))

    ;; Snippet fields navigation.
    (define-key yas-keymap (kbd "M-h") 'yas-skip-and-clear-or-delete-char)
    (define-key yas-keymap (kbd "M-t") 'yas-prev-field)
    (define-key yas-keymap (kbd "M-n") 'yas-next-field)

    ;; Don't load snippets shipped with Yasnippet.
    (setq yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))

    ;; Spacemacs disables smartparens during the yasnippet expansion, but
    ;; auto-pairing is useful in certain snippets. As a workaround, we
    ;; temporarily enable `electric-pair-mode'.
    (add-hook 'yas-before-expand-snippet-hook #'electric-pair-mode)
    (add-hook 'yas-after-exit-snippet-hook (lambda () (electric-pair-mode -1)))))

;;; packages.el ends here
