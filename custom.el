;; -*- mode: emacs-lisp -*-
;; This file is where Emacs writes custom variables.
;; Spacemacs will copy its content to your dotfile automatically in the
;; function `dotspacemacs/emacs-custom-settings'.
;; Do not alter this file, use Emacs customize interface instead.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0.25)
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(evil-move-beyond-eol t)
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments '("-l"))
 '(flycheck-idle-change-delay 5)
 '(helm-dash-browser-func 'eww)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(magit-diff-refine-hunk 'all)
 '(magit-log-arguments '("--graph" "--decorate" "-n326"))
 '(magit-log-margin '(t age-abbreviated magit-log-margin-width t 18))
 '(magit-merge-arguments '("--ff-only"))
 '(magit-pull-arguments nil)
 '(magit-rebase-arguments '("--autosquash"))
 '(mocha-options "--recursive --require babel-register")
 '(nodejs-repl-command "node")
 '(org-M-RET-may-split-line '((default)))
 '(org-agenda-files '("~/org"))
 '(org-catch-invisible-edits 'smart)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-superstar-remove-leading-stars t)
 '(package-selected-packages
   '(text-scale+ pdf-tools gcmh meson-mode toml-mode company-box frame-local ligature sphinx-doc org-rich-yank lsp-origami origami lsp-pyright ivy-avy org-superstar loop import-js treemacs-icons-dired posframe emr list-utils elisp-refs helpful magit-section treemacs-persp lispyville org-ql ov org-super-agenda ts flycheck-elsa terminal-here language-detection tide company-phpactor phpactor composer php-runtime evil-escape flycheck-ycmd company-ycmd ycmd request-deferred ivy-rtags google-c-style flycheck-rtags disaster cquery cpp-auto-include company-rtags rtags company-c-headers clang-format ccls vterm treemacs-magit drupal-mode lsp-ivy flycheck-phpstan phpstan hybrid-mode dap-mode bui tree-mode vue-mode edit-indirect ssass-mode vue-html-mode geiser erc-tweet lsp-python-ms lsp-treemacs helm-lsp eglot flymake jsonrpc devdocs dash-docs symbol-overlay org-cliplink flycheck-package package-lint evil-textobj-line blacken transient lv forge closql emacsql-sqlite emacsql sly-repl-ansi-color sly-macrostep sly color-theme-sanityinc-solarized lsp-ui lsp-python lsp-javascript-typescript typescript-mode company-lsp lsp-php lsp-mode writeroom-mode visual-fill-column ivy-rich treepy graphql prettier-js doom-modeline eldoc-eval shrink-path gitignore-templates dotenv-mode nov esxml ivy-yasnippet magit-svn json-navigator hierarchy evil-goggles lispy zoutline pipenv org-mime ivy-xref centered-cursor-mode spaceline-all-the-icons all-the-icons memoize font-lock+ pippel importmagic epc ctable concurrent deferred counsel-css overseer nameless let-alist clojure-cheatsheet wgrep symon smex slime-company mu4e-maildirs-extension ivy-purpose fuzzy flyspell-correct-ivy counsel-dash geben treemacs-projectile treemacs-evil treemacs pfuture ac-php dockerfile-mode docker tablist docker-tramp ace-jump-buffer avy-buffer-menu unfill org-category-capture counsel-projectile impatient-mode nodejs-repl org-brain evil-org company-php ac-php-core xcscope flycheck-bashate mocha magithub ghub+ apiwrap ghub common-lisp-snippets browse-at-remote swiper sayid password-generator evil-lion company-lua graphql-mode winum mu4e-alert ede-php-autoload slime evil-unimpaired counsel ivy parinfer smartscan rebox2 rcirc-notify rcirc-color racket-mode ivy-hydra sudoku restclient-helm ob-restclient company-restclient know-your-http-well insert-shebang hide-comnt helm-purpose window-purpose imenu-list pug-mode mmt powerline purescript-mode php-mode org alert log4e gntp skewer-mode simple-httpd json-snatcher json-reformat js2-mode parent-mode request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht flyspell-correct pos-tip flx grizzl magit-popup anzu evil goto-chg diminish web-completion-data dash-functional tern company hydra paredit peg eval-sexp-fu highlight seq spinner queue pkg-info clojure-mode epl bind-map bind-key yasnippet packed anaconda-mode pythonic f avy-menu avy async auto-complete popup package-build dash web-mode persp-mode org-plus-contrib neotree js2-refactor help-fns+ helm-themes helm-pydoc helm-projectile helm-gtags helm-descbinds helm-ag clj-refactor ace-jump-helm-line iedit smartparens undo-tree flycheck helm helm-core markdown-mode projectile magit git-commit s zeal-at-point yapfify yaml-mode xterm-color xkcd ws-butler with-editor window-numbering which-key web-beautify volatile-highlights vmd-mode vimrc-mode vi-tilde-fringe uuidgen use-package typit toc-org tagedit string-inflection stickyfunc-enhance srefactor sql-indent speed-type spacemacs-theme spaceline solarized-theme smeargle slim-mode simple-mpc shell-pop scss-mode sass-mode restclient restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort psci psc-ide popwin pip-requirements phpunit phpcbf php-refactor-mode php-extras php-auto-yasnippets pcre2el paradox pandoc-mode pacmacs ox-pandoc ox-gfm orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-http nginx-mode mwim mustache-mode mustache multiple-cursors multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode keyfreq json-mode js-doc jade-mode info+ inflections indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-swoop helm-mode-manager helm-make helm-gitignore helm-flx helm-dash helm-css-scss helm-company helm-c-yasnippet google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags flyspell-correct-helm flycheck-pos-tip flycheck-ledger flx-ido fish-mode fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav edn editorconfig dumb-jump dtrt-indent diff-hl define-word dactyl-mode cython-mode csv-mode crontab-mode company-web company-tern company-statistics company-shell company-anaconda column-enforce-mode color-identifiers-mode coffee-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-popup-menu ace-link ac-ispell 2048-game))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((Package . FLEXI-STREAMS)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)
     (comment-style . multi-line)
     (common-lisp-style . modern)
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (Base . 10)
     (projectile-project-type common-lisp)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (Syntax . ANSI-Common-Lisp)))
 '(vc-follow-symlinks t)
 '(warning-suppress-types '((yasnippet backquote-change)))
 '(which-key-dont-use-unicode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36"))))
 '(avy-lead-face ((t (:inherit isearch :background "#073642" :foreground "#dc322f"))))
 '(avy-lead-face-0 ((t (:inherit isearch :background "#073642" :foreground "#268bd2"))))
 '(avy-lead-face-1 ((t (:inherit isearch :background "#dc322f" :foreground "#073642"))))
 '(avy-lead-face-2 ((t (:inherit isearch :background "#268bd2" :foreground "#073642"))))
 '(button ((t (:foreground "#268bd2" :underline t))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(company-tooltip-selection ((t (:background "#0a5d43" :weight bold))))
 '(ediff-fine-diff-A ((t (:background "#8B2C02" :foreground "light steel blue"))))
 '(ediff-fine-diff-B ((t (:background "#546E00" :foreground "LightSteelBlue3"))))
 '(font-lock-comment-face ((t (:background "#073642" :foreground "#586e75"))))
 '(font-lock-doc-face ((t (:foreground "#2aa198" :slant normal))))
 '(ivy-current-match ((t (:background "#0a5d43" :weight bold))))
 '(magit-blame-date ((t (:background "#93a1a1" :foreground "firebrick4"))) t)
 '(magit-blame-name ((t (:background "#93a1a1" :foreground "firebrick4"))) t)
 '(smerge-refined-added ((t (:inherit diff-refine-added))))
 '(smerge-refined-changed ((t (:inherit diff-refine-changed))))
 '(smerge-refined-removed ((t (:inherit diff-refine-removed))))
 '(which-key-command-description-face ((t (:foreground "#268bd2"))))
 '(which-key-key-face ((t (:foreground "#b58900" :weight bold)))))
