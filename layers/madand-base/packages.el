;;; packages.el --- madand-base layer packages file for Spacemacs.
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

;; Base layer with general configuration.

;;; Code:

(defconst madand-base-packages
  '(
    ace-jump-buffer
    ace-popup-menu
    avy
    (auto-fill :location built-in)
    (browse-url :location built-in)
    centered-cursor-mode
    company
    (cus-edit :location built-in)
    dockerfile-mode
    (eshell :location built-in)
    evil
    evil-escape
    evil-evilified-state
    evil-iedit-state
    evil-magit
    evil-mc
    evil-surround
    evil-unimpaired
    flyspell
    hungry-delete
    (info :location built-in)
    (lsp-ivy :requires lsp-mode
             :location (recipe :fetcher github :repo "emacs-lsp/lsp-ivy"))
    magithub
    (man :location built-in)
    multi-term
    nov
    org
    org-pomodoro
    persp-mode
    projectile
    (python :location built-in)
    rainbow-identifiers
    (rcirc :location built-in)
    rcirc-notify
    (simple :location built-in)
    simple-mpc
    smartparens
    (smerge-mode :location built-in)
    transient
    treemacs
    undo-tree
    window-purpose
    writeroom-mode
    yaml-mode
    yasnippet
    ))

(defun madand-base/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :commands ace-jump-persp-buffers
    :init
    (spacemacs/set-leader-keys "y" #'ace-jump-persp-buffers)
    :config
    (setq ajb-sort-function #'madand//ajb-sort-function)))

(defun madand-base/init-ace-popup-menu ()
  (use-package ace-popup-menu
    :defer t
    :init
    (spacemacs/defer-until-after-user-config #'ace-popup-menu-mode)))

(defun madand-base/post-init-avy ()
  (with-eval-after-load 'avy
    (setq avy-timeout-seconds 0.2)
    ;; Bind Avy commands to s and S in Normal and Visual.
    (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-1)
    (define-key evil-motion-state-map (kbd "s") 'avy-goto-word-1)
    (define-key evil-normal-state-map (kbd "S") 'avy-goto-char-timer)
    (define-key evil-motion-state-map (kbd "S") 'avy-goto-char-timer)))

(defun madand-base/init-auto-fill ()
  ;; Activate `auto-fill-mode' by default in some major modes.
  (spacemacs/add-to-hooks #'spacemacs/toggle-auto-fill-mode-on
                          '(org-mode-hook
                            emacs-lisp-mode-hook
                            php-mode-hook
                            lisp-mode-hook)))

(defun madand-base/init-avy-buffer-menu ()
  (use-package avy-buffer-menu
    :defer t
    :init
    (spacemacs/set-leader-keys "y" #'avy-buffer-menu)
    :config
    (setq avy-buffer-menu-buffer-list-fn #'persp-buffer-list
          avy-buffer-menu-filter-fn #'spacemacs/useful-buffer-p)))

(defun madand-base/init-browse-url ()
  (setq browse-url-browser-function '(("youtube\\.com/" . madand/browse-url-mpv)
                                      ("github\\.com/" . browse-url-firefox)
                                      ("." . eww-browse-url))
        browse-url-firefox-new-window-is-tab t))

(defun madand-base/post-init-centered-cursor-mode ()
  (spacemacs/add-to-hooks #'spacemacs/toggle-centered-point-on
                          '(Man-mode-hook
                            Info-mode-hook
                            magit-revision-mode-hook
                            nov-mode-hook)))

(defun madand-base/post-init-company ()
  (with-eval-after-load 'company
    (setq company-gtags-insert-arguments nil
          company-show-numbers t
          company-selection-wrap-around t
          company-idle-delay 0.3)
    (define-key company-mode-map [C-i] 'company-complete)
    (define-key company-active-map (kbd "C-g") 'company-abort)
    (define-key company-active-map (kbd "<escape>") 'company-abort)
    (define-key company-active-map (kbd "C-/") 'counsel-company)
    (define-key company-active-map [C-i] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-t") 'company-show-doc-buffer)
    ;; Temporary disable page-break-lines-mode.
    (add-hook 'company-completion-started-hook
              #'madand//company-turn-off-page-break-lines)
    (add-hook 'company-completion-finished-hook
              #'madand//company-maybe-turn-on-page-break-lines)
    (add-hook 'company-completion-cancelled-hook
              #'madand//company-maybe-turn-on-page-break-lines)))

(defun madand-base/init-cus-edit ()
  (evilified-state-evilify-map custom-mode-map
    :mode Custom-mode
    :eval-after-load cus-edit
    :bindings
    "i" 'evil-insert-state
    "o" 'ace-link-custom
    "s" 'avy-goto-word-1
    "S" 'avy-goto-char-timer
    ))

(defun madand-base/post-init-dockerfile-mode ()
  (add-hook 'dockerfile-mode-hook #'madand/set-tab-width-4))

(defun madand-base/post-init-eshell ()
  (advice-add 'eshell-write-aliases-list :around
              #'madand-base//eshell-write-aliases-list@maybe-inhibit)
  (with-eval-after-load 'em-alias
    (unless (file-exists-p eshell-aliases-file)
      (madand-base/eshell-define-aliases))))

(defun madand-base/post-init-evil ()
  ;; Disable default use of tags to find definitions.
  (setq spacemacs-default-jump-handlers
        (delete 'evil-goto-definition spacemacs-default-jump-handlers))
  ;; Don't show Scroll Transient State full help by default.
  (setq spacemacs--scroll-ts-full-hint-toggle nil)
  (with-eval-after-load 'evil-states
    ;; Make Term buffers start in Emacs state.
    (setq evil-insert-state-modes (delq 'term-mode evil-insert-state-modes))
    (add-to-list 'evil-emacs-state-modes 'term-mode)

    (define-key evil-normal-state-map (kbd "gp") 'madand/evil-select-pasted)
    (define-key evil-normal-state-map
      (kbd "O") 'madand/evil-open-above-maybe-continue-comment)
    (define-key evil-normal-state-map
      (kbd "o") 'madand/evil-open-below-maybe-continue-comment)))

(defun madand-base/post-init-evil-escape ()
  (with-eval-after-load 'evil-escape
    (setq evil-escape-key-sequence "uh")))

(defun madand-base/post-init-evil-evilified-state ()
  (define-key evil-evilified-state-map-original
    (kbd "Q") 'spacemacs/kill-this-buffer))

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

(defun madand-base/post-init-nov ()
  (with-eval-after-load 'nov
    (cl-pushnew '(nov-mode . nov-render-document)
                madand-special-modes-re-render-functions)
    (add-hook 'nov-mode-hook #'madand//set-text-scale-for-documentation)))

(defun madand-base/post-init-org ()
  (with-eval-after-load 'org
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ec" #'org-edit-src-code)))

(defun madand-base/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (madand/pomodoro-long-mode)))

(defun madand-base/init-info ()
  (add-to-list 'Info-additional-directory-list
               (expand-file-name "~/docs/Info"))
  (add-hook 'Info-mode-hook #'madand//set-text-scale-for-documentation)
  (with-eval-after-load 'info
    (evil-define-key 'motion Info-mode-map
      "s" #'evil-avy-goto-char-timer
      (kbd "S-SPC") #'Info-scroll-up)))

(defun madand-base/init-lsp-ivy ()
  (use-package lsp-ivy
    :after lsp-mode))

(defun madand-base/post-init-magithub ()
  (with-eval-after-load 'ghub
    (setq ghub-username "madand")))

(defun madand-base/init-man ()
  (cl-pushnew '(Man-mode . Man-update-manpage)
              madand-special-modes-re-render-functions))

(defun madand-base/post-init-evil-mc ()
  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode))

(defun madand-base/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    ;; In Visual state release s and S for Avy. Use gs for surround.
    (evil-define-key 'visual evil-surround-mode-map "s" nil)
    (evil-define-key 'visual evil-surround-mode-map "S" nil)
    (evil-define-key 'visual evil-surround-mode-map "gs" 'evil-surround-region)))

(defun madand-base/post-init-evil-unimpaired ()
  (with-eval-after-load 'evil-unimpaired
    (advice-add 'evil-unimpaired/insert-space-above :override
                #'madand/insert-space-above)
    (advice-add 'evil-unimpaired/insert-space-below :override
                #'madand/insert-space-below)))

(defun madand-base/post-init-flyspell ()
  (with-eval-after-load 'flyspell
    (define-key evil-normal-state-map (kbd "zi") 'flyspell-correct-word-before-point)))

(defun madand-base/post-init-hungry-delete ()
  (spacemacs/defer-until-after-user-config #'global-hungry-delete-mode)
  (with-eval-after-load 'hungry-delete
    (spacemacs|hide-lighter hungry-delete-mode)))

(defun madand-base/post-init-google-translate ()
  (with-eval-after-load 'google-translate
    (setq google-translate-backend-method 'curl)))

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
    (evil-magit-define-key 'normal 'magit-revision-mode-map
                           (kbd "yd") 'madand/magit-copy-commit-date)
    (add-hook 'magit-status-headers-hook 'magit-insert-repo-header t)))

(defun madand-base/post-init-persp-mode ()
  (with-eval-after-load 'persp-mode
    (setq persp-auto-save-opt 0
          persp-kill-foreign-buffer-behaviour 'kill)))

(defun madand-base/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-init (setq projectile-dynamic-mode-line nil)))

(defun madand-base/post-init-python ()
  (with-eval-after-load 'python
    (add-hook 'python-mode-hook (lambda ()
                                  (setq-local zeal-at-point-docset "python")))))

(defun madand-base/post-init-rainbow-identifiers ()
  ;; Selectively enable for some programming modes.
  (spacemacs/add-to-hooks #'rainbow-identifiers-mode '(emacs-lisp-mode-hook
                                                       lisp-mode-hook
                                                       lua-mode-hook
                                                       shell-mode-hook))
  ;; Workaround colorized comments by delaying the activation.
  (spacemacs/add-to-hooks #'madand//turn-on-rainbow-identifiers-with-delay
                          '(js2-mode-hook)))

(defun madand-base/post-init-rcirc ()
  (with-eval-after-load 'rcirc
    (setq rcirc-log-directory nil
          rcirc-log-flag nil
          rcirc-default-user-name "madand"
          rcirc-buffer-maximum-lines nil
          rcirc-time-format "%H:%M "
          rcirc-fill-column 78
          rcirc-fill-prefix (make-string 6 ?\s)
          rcirc-kill-channel-buffers t)
    (evil-set-initial-state 'rcirc-mode 'normal)
    (add-hook 'rcirc-mode-hook #'madand//set-text-scale-for-irc)
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
      (let ((socket-path (expand-file-name "~/.config/mpd/socket")))
        (setq simple-mpc-arguments (concat "--host=" socket-path)))
      (setq simple-mpc-mpd-playlist-directory "~/.config/mpd/playlists/"
            simple-mpc-playlist-auto-refresh 2
            simple-mpc-playlist-format "%position%\\t%time%\\t%artist%\\t%track%\\t[%name%: ]%title%\\t%album%"
            simple-mpc-table-separator "\t")
      (evilified-state-evilify-map simple-mpc-mode-map
        :mode simple-mpc-mode
        :bindings
        (kbd "C-m") (kbd "<return>")
        "(" #'simple-mpc-decrease-volume
        ")" #'simple-mpc-increase-volume))))

(defun madand-base/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (show-smartparens-global-mode 0)))

(defun madand-base/post-init-smerge-mode ()
  (with-eval-after-load 'smerge-mode
    (add-hook 'smerge-mode-hook #'madand//smerge-disable-rainbow-identifiers)))

(defun madand-base/post-init-transient ()
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit)))

(defun madand-base/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    (define-key evil-treemacs-state-map [C-i] #'treemacs-TAB-action)))

(defun madand-base/pre-init-undo-tree ()
  (spacemacs|use-package-add-hook undo-tree
    :post-init (setq undo-tree-enable-undo-in-region nil)))

(defun madand-base/post-init-window-purpose ()
  (setq dotspacemacs-switch-to-buffer-prefers-purpose t))

(defun madand-base/post-init-writeroom-mode ()
  (add-hook 'writeroom-mode-hook #'madand//maybe-re-render-special-buffer)
  (setq writeroom-major-modes '(Man-mode Info-mode nov-mode))
  (spacemacs/defer-until-after-user-config #'global-writeroom-mode)
  (with-eval-after-load 'writeroom-mode
    (setq writeroom-fullscreen-effect 'maximized
          writeroom-maximize-window nil
          writeroom-mode-line t)))

(defun madand-base/post-init-yaml-mode ()
  (with-eval-after-load 'yaml-mode
    (add-hook 'yaml-mode-hook #'smartparens-mode)))

(defun madand-base/post-init-yasnippet ()
  (with-eval-after-load 'yasnippet
    (setq yas-prompt-functions '(yas-x-prompt) ; Force use of `ace-popup-menu'.
          ;; Disable snippet expansion in strings and comments, unless
          ;; a snippet's condition evaluates to 'force-in-comment.
          yas-buffer-local-condition '(if (madand/in-string-or-comment-p)
                                          '(require-snippet-condition . force-in-comment)
                                        t)
          ;; Don't load snippets shipped with Yasnippet.
          yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))
    ;; Fall back to completing prompt for yasnippet functions with
    ;; too many choices to be shown via `ace-popup-menu'.
    (advice-add 'yas-visit-snippet-file :around
                #'madand//yas-fallback-to-completing-prompt)
    (advice-add 'yas-insert-snippet :around
                #'madand//yas-fallback-to-completing-prompt)
    ;; Expand snippets with SPC and M-z in hybrid state.
    (evil-define-key 'hybrid yas-minor-mode-map
      (kbd "SPC") yas-maybe-expand
      (kbd "M-z") #'yas-expand)
    ;; Snippet fields navigation.
    (define-key yas-keymap (kbd "M-h") 'yas-skip-and-clear-or-delete-char)
    (define-key yas-keymap (kbd "M-t") 'yas-prev-field)
    (define-key yas-keymap (kbd "M-n") 'yas-next-field)
    ;; Compatibility with M-t and M-n during yasnippet expansion.
    (with-eval-after-load 'company
      (define-key company-active-map
        (kbd "M-t") (madand//company-maybe-call-yas-command #'yas-prev-field))
      (define-key company-active-map
        (kbd "M-n") (madand//company-maybe-call-yas-command #'yas-next-field)))
    ;; Spacemacs disables Smartparens during the yasnippet expansion, but
    ;; auto-pairing is useful in certain snippets. As a workaround, we
    ;; temporarily enable `electric-pair-mode'.
    (add-hook 'yas-before-expand-snippet-hook #'electric-pair-local-mode)
    (add-hook 'yas-after-exit-snippet-hook
              #'madand/turn-off-electric-pair-local-mode)))

;;; packages.el ends here
