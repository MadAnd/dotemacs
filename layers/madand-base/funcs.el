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

                                        ; (require 'dash)
                                        ; (require 's)

(defun madand/buffer-dir-name ()
  "Get the name of a directory where the current buffer's file is located. "
  (require 'f)
  (-> (buffer-file-name) f-dirname f-filename))

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

(defun madand/sudo-async-shell-command ()
  "Wrapper for `async-shell-command' which invokes TRAMP sudo before
the actual command."
  (interactive)
  (with-temp-buffer
    (cd "/sudo::/")
    (call-interactively #'async-shell-command)))

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


;; Translation between variable naming stysels:
;; CamelCase, underscore_case, dash-case, colon::case.
;; The following defuns are form https://www.emacswiki.org/emacs/CamelCase

(defun madand/split-name (s)
  "Split the string S into words. Supports UpperCamelCase and
lowerCamelCase formats."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun madand/ucamelcase (s)
  "Transform the string S to UpperCamelCase."
  (mapconcat 'capitalize (madand/split-name s) ""))

(defun madand/underscore (s)
  "Transform the string S to underscore_splitted_form."
  (mapconcat 'downcase   (madand/split-name s) "_"))

(defun madand/dasherize (s)
  "Transform the string S to dash-splitted-form."
  (mapconcat 'downcase   (madand/split-name s) "-"))

(defun madand/colonize (s)
  "Transform the string S to double::colon::splitted::form."
  (mapconcat 'capitalize (madand/split-name s) "::"))

(defun madand/lcamelcase  (s)
  "Transform the string S to lowerCamelCase."
  (let ((words (madand/split-name s)))
    (concat
     (car words)
     (mapconcat 'capitalize (cdr words) ""))))

(defun madand/camelscore (s)
  (let ((case-fold-search nil))
    (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	(madand/dasherize  s))
          ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	(madand/ucamelcase  s))
          ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	(madand/colonize   s))
          (t  (madand/underscore s)))))

(defun madand/cameldash (s)
  "Transform the string S depending on its current form:
- dash-delimited-text into lowerCamelCase
- underscore_delimited_text into lowerCamelCase
- lowerCamelCase into UpperCamelCase
- *anything other* into underscore_delimited_text"
  (let ((case-fold-search nil))
    (cond ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	(madand/lcamelcase s))
          ((string-match-p "^[a-z].+\\(?:[A-Z][a-z]+\\)+$"  s)	(madand/ucamelcase s))
          ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	(madand/underscore s))
          (t  (madand/dasherize s)))))

(cl-defun madand/variable-at-point (&optional (var-regex "[:alnum:]_-"))
  "Get the whole \"variable\" at point.

Optional argument VAR-REGEX defines a regexp of what will be
considered a variable."
  (save-excursion
    (let* ((beg (and (skip-chars-backward var-regex) (point)))
           (end (and (skip-chars-forward  var-regex) (point)))
           (txt (buffer-substring beg end)))
      (cl-values txt beg end))))

(defun madand/transform-variable-at-point (transformer)
  "Transform a variable at point into another form by applying TRANSFORMER.

TRANSFORMER must be a defun of one argument, the input string.

Variable at point is determined by `madand/variable-at-point'."
  (let ((point (point)))
    (cl-multiple-value-bind (txt beg end) (madand/variable-at-point)
      (let ((cml (funcall transformer txt)))
        (when cml
          (delete-region beg end)
          (insert cml)
          (set-window-point nil point))))))

(defun madand/camelscore-variable-at-point ()
  (interactive)
  (madand/transform-variable-at-point #'madand/camelscore))

(defun madand/cameldash-variable-at-point ()
  (interactive)
  (madand/transform-variable-at-point #'madand/cameldash))


(defun madand/evil-select-pasted ()
  (interactive)
  (let ((start-marker (evil-get-marker ?[))
                      (end-marker (evil-get-marker ?])))
    (evil-visual-select start-marker end-marker)))

(defun madand/insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
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


;;; funcs.el ends here
