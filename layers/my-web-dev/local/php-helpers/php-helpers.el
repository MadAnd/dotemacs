;;; -*- lexical-binding: t -*-
;;; php-helpers.el --- Helper functions for PHP editing in Emacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/syl20bnr/spacemacs
;; Based on: https://github.com/glynnforrest/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)



(defvar php-helpers-php-class-finder-script
  (concat (f-dirname load-file-name) "/bin/php_class_finder")
  "Script that extracts qualified class names in the given directory recursively.")

(defvar php-helpers--project-php-classes (make-hash-table :test 'equal :size 10)
  "Hash table of a project's path to a list of the PHP classes in the project.")



(defun php-helpers/go-to-namespace ()
  "Go to the namespace declaration or insert guessed, if not found.

Namespace is guessed by `php-helpers/namespace-from-path'."
  (interactive)
  (beginning-of-buffer)
  (unless (re-search-forward "^namespace" nil t)
    (end-of-line)
    (insert "\n")
    (insert "\n")
    (php-helpers/insert-namespace))
  (beginning-of-line))

(defun php-helpers/go-to-last-use-statement ()
  "Go to the last use statement at the top of this file."
  (interactive)
  (end-of-buffer)
  (unless (re-search-backward "^use" nil t)
    (php-helpers/go-to-namespace)
    (next-line)
    (newline)
    (previous-line))
  (recenter))



(defun php-helpers//class-list-file-name (project-root)
  "Generate class list cache file name from project root path.
Ensure that destination directory exists."
  (let ((cache-dir (concat spacemacs-cache-directory "php/")))
    (unless (f-exists? cache-dir)
      (f-mkdir cache-dir))
    (->> project-root
         (s-chop-prefix "/")
         (s-chop-suffix "/")
         (s-replace-all '(("/" . "-")))
         (s-append "-classes.txt")
         (concat cache-dir))))

(defun php-helpers//load-php-class-list (project-root &optional refresh)
  "Load PHP classes list from cache file into `php-helpers--project-php-classes'.

If cache file not exists, it will be generated.
If optional argument REFRESH is not nil, regenerate cache file befare read."
  (let ((cache-file (php-helpers//class-list-file-name project-root)))
    (when (or refresh
              (not (f-exists? cache-file)))
      (call-process-shell-command
       (concat (expand-file-name php-helpers-php-class-finder-script)
               " " project-root
               " " cache-file))
      (message "PHP classes cache refreshed for %s." project-root))
    (puthash project-root
             (split-string (f-read-text cache-file))
             php-helpers--project-php-classes)))

(defun php-helpers/class-candidates (&optional refresh)
  "Get a list of available PHP classes in the current projectile project.

If optional argument REFRESH if not nil, force refresh of the cached class list.
If called interactively, prefix argument forces refresh."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         candidates)
    (when refresh
      (setq candidates (php-helpers//load-php-class-list project-root refresh)))
    (or candidates
        (gethash project-root
                 php-helpers--project-php-classes
                 (php-helpers//load-php-class-list project-root refresh)))))

(defun php-helpers/insert-use-class (&optional refresh)
  "Add a class to the use declarations in the current file.
With prefix argument refresh cache before listing candidates."
  (interactive "P")
  (save-restriction
    (save-excursion
      (widen)
      (let ((class (helm-comp-read
                    "Class: "
                    (php-helpers/class-candidates refresh)
                    :must-match t)))
        (php-helpers/go-to-last-use-statement)
        (end-of-line)
        (newline)
        (insert (concat "use " class ";"))))))

(defun php-helpers/insert-class (&optional refresh)
  "Insert a class name from the current projectile project.
With prefix argument refresh cache before listing candidates."
  (interactive "P")
  (let ((class (helm-comp-read "Class: "
                               (php-helpers/class-candidates refresh)
                               :must-match t)))
    (insert (concat "\\" class))))

(defun php-helpers/insert-namespace (&optional do-not-guess)
  "Insert a namespace guessed from current file path."
  (interactive)
  (insert (format "namespace %s;\n" (php-helpers/current-file-namespace))))



(defun php-helpers/current-file-namespace ()
  "Guess a namespace from a path of the current file."
  (interactive)
  (cond
   ((s-contains? "/src/" buffer-file-name)
    (php-helpers/namespace-from-path buffer-file-name "/src/"))
   ((s-contains? "/tests/" buffer-file-name)
    (php-helpers/namespace-from-path buffer-file-name "/tests/"))
   ((s-contains? "/app/" buffer-file-name)
    (php-helpers/namespace-from-path buffer-file-name "/app/"))
   (t (php-helpers/namespace-from-path buffer-file-name
                                           (projectile-project-root)))))

(defun php-helpers/namespace-from-path (path substr)
  "Extract a namespace from a path name that contains `substr`."
  (s-with (substring path (+ (s-index-of substr path) (length substr)))
    (s-split "/")
    butlast
    (s-join "\\")))

(provide 'php-helpers)

;;; php-helpers.el ends here
