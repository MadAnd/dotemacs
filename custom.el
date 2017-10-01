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
 '(ede-project-directories (quote ("/home/madand/web/scribble.io")))
 '(elm-indent-after-keywords
   (quote
    (("of" 2)
     ("in" 2 0)
     ("{" 2)
     "if" "then" "else" "let")))
 '(elm-indent-offset 2)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-modules
   (quote
    (autojoin completion log match move-to-prompt services spelling image hl-nicks youtube services netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-prompt-for-password nil)
 '(evil-move-beyond-eol t)
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(flycheck-idle-change-delay 5)
 '(helm-dash-browser-func (quote eww))
 '(magit-diff-refine-hunk (quote all))
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n326")))
 '(magit-log-margin (quote (t age-abbreviated magit-log-margin-width t 18)))
 '(magit-merge-arguments (quote ("--ff-only")))
 '(magit-pull-arguments nil)
 '(magit-rebase-arguments (quote ("--autosquash")))
 '(mocha-options "--recursive --require babel-register")
 '(nodejs-repl-command "node")
 '(org-M-RET-may-split-line (quote ((default))))
 '(org-agenda-files (quote ("~/org")))
 '(org-catch-invisible-edits (quote smart))
 '(org-pomodoro-length 40)
 '(org-pomodoro-long-break-length 40)
 '(org-pomodoro-short-break-length 10)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(package-selected-packages
   (quote
    (parinfer smartscan rebox2 rcirc-notify rcirc-color racket-mode ivy-hydra prog-inflection sudoku restclient-helm ob-restclient company-restclient know-your-http-well insert-shebang hide-comnt helm-purpose window-purpose imenu-list pug-mode mmt powerline purescript-mode php-mode org alert log4e gntp skewer-mode simple-httpd json-snatcher json-reformat js2-mode parent-mode request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht flyspell-correct pos-tip flx grizzl magit-popup anzu evil goto-chg diminish web-completion-data dash-functional tern company hydra paredit peg eval-sexp-fu highlight seq spinner queue pkg-info clojure-mode epl bind-map bind-key yasnippet packed anaconda-mode pythonic f avy-menu avy async auto-complete popup package-build dash web-mode persp-mode org-plus-contrib neotree js2-refactor help-fns+ helm-themes helm-pydoc helm-projectile helm-gtags helm-descbinds helm-ag clj-refactor ace-jump-helm-line iedit smartparens undo-tree flycheck helm helm-core markdown-mode projectile magit git-commit s zeal-at-point yapfify yaml-mode xterm-color xkcd ws-butler with-editor window-numbering which-key web-beautify volatile-highlights vmd-mode vimrc-mode vi-tilde-fringe uuidgen use-package typit toc-org tagedit string-inflection stickyfunc-enhance srefactor sql-indent speed-type spacemacs-theme spaceline solarized-theme smeargle slim-mode simple-mpc shell-pop scss-mode sass-mode restclient restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort psci psc-ide popwin pip-requirements phpunit phpcbf php-refactor-mode php-extras php-auto-yasnippets pcre2el paradox pandoc-mode pacmacs ox-pandoc ox-gfm orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-http nginx-mode mwim mustache-mode mustache multiple-cursors multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode keyfreq json-mode js-doc jade-mode info+ inflections indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-swoop helm-mode-manager helm-make helm-gitignore helm-flx helm-dash helm-css-scss helm-company helm-c-yasnippet google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags flyspell-correct-helm flycheck-pos-tip flycheck-ledger flx-ido fish-mode fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav edn editorconfig dumb-jump dtrt-indent diff-hl define-word dactyl-mode cython-mode csv-mode crontab-mode company-web company-tern company-statistics company-shell company-anaconda column-enforce-mode color-identifiers-mode coffee-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-popup-menu ace-link ac-ispell 2048-game)))
 '(paradox-github-token t)
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (less-css-compile-at-save . t)
     (zeal-at-point-docset "yiiphp")
     (php-auto-yasnippet-required-files
      (list "~/web/scribble.io/vendor/autoload.php"))
     (eval php-enable-drupal-coding-style)
     (eval php-enable-psr2-coding-style)
     (eval php-enable-symfony2-coding-style)
     (checkdoc-minor-mode . t)
     (firestarter . ert-run-tests-interactively))))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:inherit isearch :background "#073642" :foreground "#dc322f"))))
 '(avy-lead-face-0 ((t (:inherit isearch :background "#073642" :foreground "#268bd2"))))
 '(avy-lead-face-1 ((t (:inherit isearch :background "#dc322f" :foreground "#073642"))))
 '(avy-lead-face-2 ((t (:inherit isearch :background "#268bd2" :foreground "#073642"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(ediff-fine-diff-A ((t (:background "#8B2C02" :foreground "light steel blue"))))
 '(ediff-fine-diff-B ((t (:background "#546E00" :foreground "LightSteelBlue3"))))
 '(font-lock-comment-face ((t (:background "#073642" :foreground "#586e75"))))
 '(ivy-current-match ((t (:background "#0a5d43" :weight bold))))
 '(magit-blame-date ((t (:background "#93a1a1" :foreground "firebrick4"))))
 '(magit-blame-name ((t (:background "#93a1a1" :foreground "firebrick4"))))
 '(smerge-refined-added ((t (:inherit diff-refine-added))))
 '(smerge-refined-changed ((t (:inherit diff-refine-changed))))
 '(smerge-refined-removed ((t (:inherit diff-refine-removed)))))
