;;; packages.el --- madand-base layer packages file for Spacemacs.Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
  '(ace-jump-buffer
    ace-popup-menu
    avy
    ;; (avy-buffer-menu :location (recipe :fetcher local))
    (browse-url :location built-in)
    company
    evil
    evil-evilified-state
    evil-iedit-state
    evil-magit
    evil-mc
    evil-surround
    flyspell
    (info :location built-in)
    info+
    magithub
    multi-term
    persp-mode
    (prog-inflection :location (recipe :fetcher local))
    (python :location built-in)
    (rcirc :location built-in)
    rcirc-notify
    (simple :location built-in)
    simple-mpc
    (smerge-mode :location built-in)
    treemacs
    ;; (sql-indent :location (recipe :fetcher github :repo "alex-hhh/emacs-sql-indent"))
    yasnippet))

(defun madand-base/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :commands ace-jump-persp-buffers
    :init
    (spacemacs/set-leader-keys "y" #'ace-jump-persp-buffers)
    :config
    (setq ajb-sort-function #'madand//ajb-sort-function)))

(defun madand-base/init-ace-popup-menu ()
  (spacemacs/defer-until-after-user-config
   (lambda ()
     (ace-popup-menu-mode 1))))

(defun madand-base/post-init-avy ()
  (with-eval-after-load 'avy
    ;; Bind Avy commands to s and S in Normal and Visual.
    (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-1)
    (define-key evil-motion-state-map (kbd "s") 'avy-goto-word-1)
    (define-key evil-normal-state-map (kbd "S") 'avy-goto-char-timer)
    (define-key evil-motion-state-map (kbd "S") 'avy-goto-char-timer)))

(defun madand-base/init-avy-buffer-menu ()
  (use-package avy-buffer-menu
    :defer t
    :init
    (spacemacs/set-leader-keys
      "y" #'avy-buffer-menu)
    :config
    (setq avy-buffer-menu-buffer-list-fn #'persp-buffer-list
          avy-buffer-menu-filter-fn #'spacemacs/useful-buffer-p)))

(defun madand-base/init-browse-url ()
  (setq browse-url-browser-function #'madand/browse-url-palemoon))

(defun madand-base/post-init-company ()
  (with-eval-after-load 'company
    (setq company-gtags-insert-arguments nil
          company-show-numbers t
          company-selection-wrap-around t)
    (define-key company-active-map (kbd "C-g") 'company-abort)
    (define-key company-active-map (kbd "<escape>") 'company-abort)
    (define-key company-active-map (kbd "C-/") 'counsel-company)
    (define-key company-active-map [C-i] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-t") 'company-show-doc-buffer)
    ;; Compatibility with M-t and M-n for inside `yasnippet' snippets.
    (define-key company-active-map (kbd "M-t") (lambda ()
                                                 (interactive)
                                                 (when (yas--active-field-overlay)
                                                   (company-abort)
                                                   (yas-prev-field))))
    (define-key company-active-map (kbd "M-n") (lambda ()
                                                 (interactive)
                                                 (when (yas--active-field-overlay)
                                                   (company-abort)
                                                   (yas-next-field))))
    ;; Workaround for `page-break-lines-mode'
    (add-hook 'company-completion-started-hook 'madand//company-turn-off-page-break-lines)
    (add-hook 'company-completion-finished-hook 'madand//company-maybe-turn-on-page-break-lines)
    (add-hook 'company-completion-cancelled-hook 'madand//company-maybe-turn-on-page-break-lines)))

(defun madand-base/post-init-evil ()
  (with-eval-after-load 'evil-states
    ;; Make Term buffers start in Emacs state.
    (setq evil-insert-state-modes (delq 'term-mode evil-insert-state-modes))
    (add-to-list 'evil-emacs-state-modes 'term-mode)

    (define-key evil-normal-state-map (kbd "gp") 'madand/evil-select-pasted)
    (define-key evil-normal-state-map (kbd "O") 'madand/evil-open-above-maybe-continue-comment)
    (define-key evil-normal-state-map (kbd "o") 'madand/evil-open-below-maybe-continue-comment)))

(defun madand-base/post-init-evil-evilified-state ()
  (define-key evil-evilified-state-map-original (kbd "Q") 'spacemacs/kill-this-buffer))

(defun madand-base/post-init-evil-iedit-state ()
  (with-eval-after-load 'evil-iedit-state
    ;; Iedit terminates whenever keyboard macro is executed, so we must use
    ;; concrete commands.
    (define-key evil-iedit-insert-state-map (kbd "C-h") 'delete-backward-char)))

(defun madand-base/post-init-evil-magit ()
  (with-eval-after-load 'evil-magit
    (evil-magit-define-key 'normal 'magit-mode-map "yB" 'madand/magit-copy-section-value-short)))

(defun madand-base/post-init-multi-term ()
  (with-eval-after-load 'multi-term
    (add-to-list 'term-bind-key-alist '("C-c z" . term-stop-subjob))
    (add-to-list 'term-bind-key-alist '("<escape>" . term-send-esc))))

(defun madand-base/init-info ()
  (setq Info-additional-directory-list (list (expand-file-name "~/.spacemacs.d/info/"))))

(defun madand-base/post-init-info+ ()
  (with-eval-after-load 'info+
    (define-key Info-mode-map (kbd "S-SPC") 'Info-scroll-up)))

(defun madand-base/post-init-magithub ()
  (with-eval-after-load 'ghub
    (setq ghub-username "madand")))

(defun madand-base/post-init-evil-mc ()
  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode))

(defun madand-base/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    ;; In Visual state release s and S for Avy. Use gs for surround.
    (evil-define-key 'visual evil-surround-mode-map "s" nil)
    (evil-define-key 'visual evil-surround-mode-map "S" nil)
    (evil-define-key 'visual evil-surround-mode-map "gs" 'evil-surround-region)))

(defun madand-base/post-init-flyspell ()
  (with-eval-after-load 'flyspell
    (define-key evil-normal-state-map (kbd "zi") 'flyspell-correct-word-before-point)))

(defun madand-base/init-jit-lock ()
  (with-eval-after-load 'jit-lock
    (setq jit-lock-defer-time nil
          jit-lock-stealth-time nil)))

(defun madand-base/post-init-magit ()
  (with-eval-after-load 'magit
    (setq magit-diff-refine-hunk 'all
          magit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ai
Commit:     %cN <%cE>
CommitDate: %ci\n")
    (evil-magit-define-key 'normal 'magit-revision-mode-map (kbd "yd") 'madand/magit-copy-commit-date)
    (add-hook 'magit-status-headers-hook 'magit-insert-repo-header t)))

(defun madand-base/post-init-persp-mode ()
  (with-eval-after-load 'persp-mode
    (setq persp-auto-save-opt 0)))

(defun madand-base/init-prog-inflection ()
  (use-package prog-inflection
    :defer nil))

(defun madand-base/post-init-python ()
  (with-eval-after-load 'python
    (add-hook 'python-mode-hook (lambda ()
                                  (setq-local zeal-at-point-docset "python")))))

(defun madand-base/post-init-rcirc ()
  (with-eval-after-load 'rcirc
    (setq rcirc-log-flag t
          rcirc-buffer-maximum-lines nil
          rcirc-fill-column (window-text-width)
          rcirc-fill-prefix (make-string 17 ?\s)
          rcirc-kill-channel-buffers t
          rcirc-time-format "%H:%M ")
    (advice-add 'rcirc-connect :around #'madand//rcirc-connect)))

(defun madand-base/post-init-rcirc-notify ()
  (with-eval-after-load 'rcirc-notify
    (setq rcirc-notify-popup-timeout 60000)))

(defun madand-base/init-simple ()
  (with-eval-after-load 'simple
    (add-hook 'find-file-hook #'madand//maybe-disable-fly-modes)))

(defun madand-base/init-simple-mpc ()
  (use-package simple-mpc
    :defer t
    :commands simple-mpc-view-current-playlist
    :init
    (spacemacs/set-leader-keys "am" #'simple-mpc-view-current-playlist)
    :config
    (progn
      (spacemacs|hide-lighter simple-mpc-current-playlist-mode)
      (setq simple-mpc-mpd-playlist-directory "~/.config/mpd/playlists/"
            simple-mpc-playlist-auto-refresh 2
            simple-mpc-playlist-format "[%track%. ][%name%: ][%title%][ - %%artist%][ - %album%]")
      (evilified-state-evilify-map simple-mpc-mode-map
        :mode simple-mpc-mode
        :bindings (kbd "C-m") (kbd "<return>")))))

(defun madand-base/post-init-smerge-mode ()
  (with-eval-after-load 'smerge-mode
    (add-hook 'smerge-mode-hook #'madand//smerge-disable-rainbow-identifiers)))

(defun madand-base/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    (define-key evil-treemacs-state-map [C-i] #'treemacs-push-button)))

(defun madand-base/post-init-yasnippet ()
  (with-eval-after-load 'yasnippet

    (setq yas-prompt-functions '(yas-x-prompt) ; Force use of `ace-popup-menu'.
          ;; Unconditionally disable snippet expansion in strings and comments.
          yas-buffer-local-condition '(not (sp-point-in-string-or-comment))
          ;; Don't load snippets shipped with Yasnippet.
          yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))
    ;; Fall back to completing prompt for yasnippet functions with
    ;; too many choices to be shown via `ace-popup-menu'.
    (advice-add 'yas-visit-snippet-file :around
                #'madand/yas-fallback-to-completing-prompt)
    (advice-add 'yas-insert-snippet :around
                #'madand/yas-fallback-to-completing-prompt)
    ;; Expand snippets with SPC.
    (evil-define-key 'hybrid yas-minor-mode-map
      (kbd "SPC") yas-maybe-expand
      (kbd "M-z") #'yas-expand)
    ;; Snippet fields navigation.
    (define-key yas-keymap (kbd "M-h") 'yas-skip-and-clear-or-delete-char)
    (define-key yas-keymap (kbd "M-t") 'yas-prev-field)
    (define-key yas-keymap (kbd "M-n") 'yas-next-field)
    ;; Spacemacs disables smartparens during the yasnippet expansion, but
    ;; auto-pairing is useful in certain snippets. As a workaround, we
    ;; temporarily enable `electric-pair-mode'.
    (add-hook 'yas-before-expand-snippet-hook #'electric-pair-mode)
    (add-hook 'yas-after-exit-snippet-hook (lambda () (electric-pair-mode -1)))))

;;; packages.el ends here
