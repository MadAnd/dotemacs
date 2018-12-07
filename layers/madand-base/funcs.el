;;; funcs.el --- madand-base layer packages file for Spacemacs.
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

;; General helper functions configuration.

;;; Code:

(defun madand/buffer-dir-name ()
  "Get the name of a directory where the current buffer's file is located. "
  (require 'f)
  (thread-first (buffer-file-name) f-dirname f-filename))

(defun madand/browse-url-palemoon (url &optional new-window)
  "Use Palemoon as the external browser."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "palemoon " url) nil
           "palemoon"
           (append
            browse-url-firefox-arguments
            (unless (memq system-type '(windows-nt ms-dos))
              (if (browse-url-maybe-new-window new-window)
                  (if browse-url-firefox-new-window-is-tab
                      '("-new-tab")
                    '("-new-window"))))
            (list url)))))

(defun madand/yas-fallback-to-completing-prompt (orig-fun &rest args)
  "Around advice that temporarily binds `yas-prompt-functions' to
'(yas-completing-prompt)."
  (let ((yas-prompt-functions '(yas/completing-prompt)))
    (apply orig-fun args)))

(defun madand/sudo-async-shell-command (command &optional output-buffer error-buffer)
  "Wrapper for `async-shell-command' which invokes TRAMP sudo before
the actual command."
  (interactive "MShell command (root): ")
  (with-temp-buffer
    (cd "/sudo:root@localhost:/")
    (async-shell-command command output-buffer error-buffer)))

(defun madand/file-set-executable ()
  "Make current file executable."
  (interactive)
  (let* ((filename (buffer-file-name))
         (orig-mode (or (file-modes filename) (error "File not found."))))
    (chmod filename (file-modes-symbolic-to-number "+x" orig-mode))))

(defun madand//smerge-disable-rainbow-identifiers ()
  "Disable `rainbow-identifiers-mode' while `smerge-mode' is active."
  (let ((current-buffer (current-buffer)))
    (run-with-idle-timer
     2 nil
     (lambda ()
       (with-current-buffer current-buffer
         (when (bound-and-true-p 'rainrainbow-identifiers-mode)
           (if (bound-and-true-p 'smerge-mode)
               (spacemacs/toggle-rainbow-identifier-off)
             (spacemacs/toggle-rainbow-identifier-on))))))))



(defun madand/evil-select-pasted ()
  "Visually select the most recently pasted region."
  (interactive)
  (let ((start-marker (evil-get-marker (string-to-char "[")))
        (end-marker (evil-get-marker (string-to-char "]"))))
    (evil-visual-select start-marker end-marker)))

(defun madand/insert-date (prefix)
  "Insert the current date.
With a universal-argument, use ISO format.
With double universal-argument, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))



(defun madand/magit-copy-commit-date (&optional full)
  "Copy commit date from Magit buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward "CommitDate")
    (beginning-of-line)
    (evil-execute-macro 1 "yy")
    (with-temp-buffer
      (yank)
      (beginning-of-line)
      (forward-evil-WORD)
      (delete-region 1 (point))
      (forward-evil-WORD)
      (if full
          (copy-region-as-kill 2 (point-max))
        (copy-region-as-kill 2 (point))))))


(defun madand/magit-tag-create-prefixed (prefix &optional delete-other-tags?)
  "Create new tag, named after the PREFIX, on the commit at point.
Auto-increment number will be appended after the prefix.

If called with prefix argument, other tags will be deleted from the commit."
  (interactive "s\nP")
  (require 'dash)
  (require 's)
  (when delete-other-tags?
    (madand/magit-tag-delete-all))
  (let* ((similar-tags (--filter (s-starts-with? prefix it) (magit-list-tags)))
         (numbers (--map (string-to-number (s-replace prefix "" it))
                         similar-tags))
         (max-number (-reduce-from 'max 0 numbers))
         (new-tag (concat prefix (number-to-string (1+ max-number)))))
    (magit-tag new-tag (magit-commit-at-point))))

(defun madand/magit-tag-delete-all-prefixed (prefix)
  "Delete all tags having names staring with PREFIX."
  (interactive "sTag prefix: ")
  (when (string-empty-p prefix)
    (user-error "Prefix cannot be empty!"))
  (let ((tags-deleted (-some->> (magit-list-tags)
                                (--filter (s-starts-with? prefix it))
                                (--map (magit-tag-delete it)))))
    (message "Deleted %d tags." (length tags-deleted))))

(defun madand/magit-tag-delete-all (&optional ignore)
  "Delete all tags from the commit at point."
  (interactive "p")
  ;; Prompt for confirmation if called interactively.
  (when (or (not ignore)
            (y-or-n-p "Really delete all tags from the commit?"))
    (while (when-let (tag (magit-tag-at-point))
             (magit-tag-delete tag)
             tag))))


(defvar madand-magit-tag-prefix-todo "pm-todo-"
  "Prefix for tags created by `madand/magit-tag-create-todo'.")

(defun madand/magit-tag-create-todo (&optional delete-other-tags?)
  "Create new auto-increment tag on the commit at point. Tag name will be
created from the value of `madand-magit-tag-prefix-todo'.

If called with prefix argument, other tags will be deleted from the commit."
  (interactive "P")
  (madand/magit-tag-create-prefixed madand-magit-tag-prefix-todo
                                    delete-other-tags?))

(defvar madand-magit-tag-prefix-done "pm-done-"
  "Create new auto-increment tag on the commit at point. Tag name will be
created from the value of `madand-magit-tag-prefix-done'.")

(defun madand/magit-tag-create-done (&optional delete-other-tags?)
  "Create new auto-increment tag on the commit at point. Tag name will be
created from the value of `madand-magit-tag-prefix-done'.

If called with prefix argument, other tags will be deleted from the commit."
  (interactive "P")
  (madand/magit-tag-create-prefixed madand-magit-tag-prefix-done
                                    delete-other-tags?))

(defvar madand-magit-tag-prefix-tbreverted "pm-revert-"
  "Create new auto-increment tag on the commit at point. Tag name will be
created from the value of `madand-magit-tag-prefix-tbreverted'.")

(defun madand/magit-tag-create-tbreverted (&optional delete-other-tags?)
  "Create new auto-increment tag on the commit at point. Tag name will be
created from the value of `madand-magit-tag-prefix-tbreverted'.

If called with prefix argument, other tags will be deleted from the commit."
  (interactive "P")
  (madand/magit-tag-create-prefixed madand-magit-tag-prefix-tbreverted
                                    delete-other-tags?))

(defun madand/magit-copy-section-value-short ()
  "Copy short (7 chars) version of the current section's value. Useful for
copying the short revision."
  (interactive)
  (-some--> (magit-copy-section-value)
            (substring it 0 7)
            (message "%s" it)
            (kill-new it t)))

(defun madand/magit-fetch-gh-pr (remote pr-num)
  "Fetch the Github PR PR-NUM from the REMOTE into branch named pr-%n."
  (interactive (list (completing-read "Remote: " (magit-list-remotes))
                     (read-from-minibuffer "Pull request number: ")))
  (magit-fetch-refspec remote
                       (format "refs/pull/%s/head:pr-%s" pr-num pr-num)
                       (magit-fetch-arguments)))



(defun madand/ediff-current-file-pacnew ()
  "Ediff the current file with newer version proposed by Pacman.
New version has the .pacnew suffix in filename."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (pacnew-file (concat current-file ".pacnew")))
    (ediff-files current-file pacnew-file)))

(defun madand/delete-pacnew-file ()
  "Delete a .pacnew file corresponding to current visited file, if any"
  (interactive)
  (let ((pacnew-file (concat (buffer-file-name) ".pacnew")))
    (if (file-exists-p pacnew-file)
        (progn
          (delete-file pacnew-file)
          (when-let (buffer (get-file-buffer pacnew-file))
            (kill-buffer buffer))
          (message "%s was deleted." pacnew-file))
      (message ".pacnew file not exists!"))))



(defvar-local madand--company-page-break-lines-mode-on-p nil)

(defun madand//company-turn-off-page-break-lines (&rest ignore)
  (when (boundp 'page-break-lines-mode)
    (setq madand--company-page-break-lines-mode-on-p page-break-lines-mode)
    (when page-break-lines-mode (page-break-lines-mode -1))))

(defun madand//company-maybe-turn-on-page-break-lines (&rest ignore)
  (when madand--company-page-break-lines-mode-on-p (page-break-lines-mode 1)))



(defun madand/disable-modes (mode-list)
  "Disable modes given in a list MODE-LIST."
  (dolist (mode mode-list)
    (and (boundp mode) (funcall mode -1))))

(defun madand//maybe-disable-fly-modes ()
  "Disable `flycheck-mode' (flyc) and/or `flyspell-mode' (flys) in the buffer,
when certain conditions are met:

* If the file is in node_modules sub-directory, disable both flyc and flys"
  (madand/disable-modes
   (catch 'disable-modes-list
     (let ((file-name (buffer-file-name)))
       ;; Check whether the file is in the node_modules sub-directory.
       (when (and file-name
                  (s-contains-p "node_modules" file-name))
         (message "Disabling flycheck and flyspell, because the file is under node_modules")
         (throw 'disable-modes-list '(flycheck-mode flyspell-mode)))
       )
     ;; The default return value - do not disable anything.
     nil)))

(defun madand/inside-multilne-c-comment-p ()
  "Return t if the point is inside of a mutilne C style \"/** … **/\" comment.
Rturns nil if point is on the comment closing part \"**/\"."
  (save-excursion
    (back-to-indentation)
    (and (madand/in-string-or-comment-p)
         (looking-at-p (rxt-pcre-to-elisp "/\\*{2}|\\*[^/]+")))))

(defun madand/evil-open-above-maybe-continue-comment (count)
  "Call `evil-open-above' then continue multi-line C-style comment (/**)
if we were inside of one."
  (interactive "p")
  (let ((should-continue-comment (madand/inside-multilne-c-comment-p)))
    (evil-open-above count)
    (when should-continue-comment
      (insert "* "))))

(defun madand/evil-open-below-maybe-continue-comment (count)
  "Call `evil-open-below' then continue multi-line C-style comment (/**)
if we were inside of one."
  (interactive "p")
  (let ((should-continue-comment (madand/inside-multilne-c-comment-p)))
    (evil-open-below count)
    (when should-continue-comment
      (insert "* "))))



(defun madand//rcirc-connect (f server &optional port nick user-name
                                full-name startup-channels password encryption
                                server-alias)
  "Get SERVER credentials from ~/.authinfo.gpg and pass them to `rcirc-connect'."
  (require 'auth-source)
  (when-let ((search-result (auth-source-search
                             :host server
                             :port '("nickserv" "bitlbee" "quakenet")
                             :require '(:port :user :secret)
                             :max 1))
             (auth-item (first search-result)))
    (setq user-name (plist-get auth-item :user))
    (setq password (funcall (plist-get auth-item :secret))))
  (funcall f server port nick user-name full-name startup-channels password
           encryption server-alias))



(defun madand//ajb-sort-function (b1 b2)
  "Sort function for `ajb-sort-function'.

If in perspective, use `bs--sort-by-name'. Otherwise, use
`bs--sort-by-recentf'."
  (if (get-current-persp)
      (bs--sort-by-name b1 b2)
    nil))

(defun madand/turn-off-electric-pair-mode ()
  "Turn off `electric-pair-mode'."
  (interactive)
  (electric-pair-mode -1))


(defun madand/in-string-p ()
  (nth 3 (syntax-ppss)))

(defun madand/in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun madand/in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))



(defun madand/pomodoro-short-mode ()
  "Configure Pomodoro to work in short mode:
25 (mins) work, 5 - short brake, 30 - long brake after 4 pomodoros."
  (interactive)
  (setq
   org-pomodoro-long-break-frequency 4
   org-pomodoro-length 25
   org-pomodoro-long-break-length 30
   org-pomodoro-short-break-length 5))

(defun madand/pomodoro-long-mode ()
  "Configure Pomodoro to work in long mode:
52 (mins) work, 17 - short brake, 30 - long brake after 4 pomodoros."
  (interactive)
  (setq
   org-pomodoro-long-break-frequency 4
   org-pomodoro-length 52
   org-pomodoro-long-break-length 30
   org-pomodoro-short-break-length 17))


;;; funcs.el ends here
