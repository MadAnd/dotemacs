;;; php-helpers.el --- Helper functions for PHP editing in Emacs.  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2015-2018 Andriy Kmit'
;;
;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/dotemacs
;; Based on: https://github.com/glynnforrest/emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (dash "2.13.0") (f "0.19.0") (s "1.12.0") (json "1.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'json)
(require 's)
(require 'thingatpt)



(defvar php-helpers-php-class-finder-script
  (concat (f-dirname load-file-name) "/bin/php_class_finder")
  "Script that extracts qualified class names in the given directory recursively.")

(defvar php-helpers--project-php-classes (make-hash-table :test 'equal :size 10)
  "Hash table of a project's path to a list of the PHP classes in the project.")

(defvar php-helpers-sort-uses-on-save nil
  "Whether use-statements should be automatically sorted on each buffer save.")
(put 'php-helpers-sort-uses-on-save 'safe-local-variable #'booleanp)

(defvar php-helpers-default-psr4-autoloads '((App\\ . "src/")
                                                  (App\\Tests\\ . "tests/"))
  "Fallback configuration for namespace inferring.

This will be used if composer.json matching failed.

The structure mirrors the result of parsing the \"autoload\".\"psr-4\" section
of the composer.json, so we can use the same matcher code.")
(put 'php-helpers-default-psr4-autoloads 'safe-local-variable #'listp)



;; Borrowed this macro from `evil-mc'.
(defmacro php-helpers|save-window-scroll (&rest forms)
  "Saves and restores the window scroll position"
  (let ((point (cl-gensym "point"))
        (win-start (cl-gensym "win-start"))
        (win-hscroll (cl-gensym "win-hscroll")))
    `(let ((,point (set-marker (make-marker) (point)))
           (,win-start (set-marker (make-marker) (window-start)))
           (,win-hscroll (window-hscroll)))
       ,@forms
       (goto-char ,point)
       (set-window-start nil ,win-start t)
       (set-window-hscroll nil ,win-hscroll))))



(defun php-helpers/go-to-namespace ()
  "Go to the namespace declaration or insert inferred, if not found.

Namespace is inferred by `php-helpers/file-path->psr4-namespace'."
  (interactive)
  (beginning-of-buffer)
  (unless (re-search-forward "^namespace" nil t)
    (end-of-line)
    (insert "\n")
    (insert "\n")
    (php-helpers/insert-namespace-declaration))
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

(defun php-helpers/do-insert-use-class (&optional refresh)
  "Add a class to the use declarations in the current file.

Prompt for a class name with `helm'.
With prefix argument refresh the class cache before listing candidates."
  (interactive "P")
  (php-helpers/insert-use-class
   (helm-comp-read
    "Class: "
    (php-helpers/class-candidates refresh)
    :must-match t)))

(defun php-helpers/do-insert-use-class-region-or-symbol (&optional refresh)
  "Add a class to the use declarations in the current file.

Prompt for a class name with `helm' with region or symbol at point pre-filled.
With prefix argument refresh the class cache before listing candidates."
  (interactive "P")
  (php-helpers/insert-use-class
   (ivy-completing-read
    "Class: "
    (php-helpers/class-candidates refresh)
    nil t
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (if-let (symbol (symbol-at-point))
          (symbol-name (symbol-at-point))
        "")))))

(defun php-helpers/insert-use-class (class-fqn)
  "Add the fully qualified class name CLASS-FQN to the use declaration in the
current buffer."
  (php-helpers|save-window-scroll
   (save-excursion
     (save-restriction
       (widen)
       (php-helpers/go-to-last-use-statement)
       (end-of-line)
       (newline)
       (insert (concat "use " class-fqn ";"))))))

;;;###autoload
(defun php-helpers/insert-class (&optional refresh)
  "Insert a class name from the current projectile project.
With prefix argument refresh cache before listing candidates."
  (interactive "P")
  (let ((class (ivy-completing-read "Class: "
                                    (php-helpers/class-candidates refresh)
                                    nil t)))
    (insert (concat "\\" class))))

;;;###autoload
(defun php-helpers/insert-namespace-declaration ()
  "Insert a namespace inferred from the current file path."
  (interactive)
  (insert (format "namespace %s;\n" (php-helpers/infer-file-namespace))))

;;;###autoload
(defun php-helpers/sort-uses ()
  "Sort file use-statements alphabetically."
  (interactive)
  (php-helpers|save-window-scroll
   (save-excursion
     (save-restriction
       (widen)
       (beginning-of-buffer)
       (when (and (derived-mode-p 'php-mode)
                  (re-search-forward "^use" nil t))
         (beginning-of-line)
         (let ((sort-fold-case t)
               (sort-start (point)))
           (forward-paragraph)
           (sort-lines nil sort-start (point))))))))

(defun php-helpers/sort-uses-after-save-handler ()
  "Handler for `after-save-hook' to automatically sort use-statements, iff
  `php-helpers-sort-uses-on-save' is t."
  (when php-helpers-sort-uses-on-save
    (php-helpers/sort-uses)))

(defun php-helpers/register-sort-uses-after-save ()
  "Register `php-helpers/sort-uses-after-save-handler' in buffer local
  `after-save-hook'."
  (add-hook 'after-save-hook #'php-helpers/sort-uses-after-save-handler nil t))



(defun php-helpers//assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun php-helpers//parse-composer-psr4-config (project-root)
  "Parse composer.json and load PSR-4 autoload definitions.

Two sections of the composer.json are considered: \"autoloads\" as well as
\"autoloads-dev\" and the result will contain data from both, if any.

Consult `php-helpers-default-psr4-autoloads' to see how the parsed data looks in
Lisp."
  (when-let* ((composer.json-file (concat project-root "composer.json"))
              (_ (file-exists-p composer.json-file))
              (json-data (json-read-file composer.json-file)))
    (append (php-helpers//assoc-recursive json-data 'autoload 'psr-4)
            (php-helpers//assoc-recursive json-data 'autoload-dev 'psr-4))))

(defun php-helpers//find-matching-prs4-config (file-relpath psr4-configs)
  "Find the first matching PSR-4 autoload configuration for FILE-RELPATH.

PSR4-CONFIGS should contain candidates in format compaitable with parsed section
\"autoload\".\"psr-4\" of the composer.json - an alist of pairs
`(psr4-ns . psr4-path)'. Where psr4-ns is a symbol that designates a prefix for
the resulting PHP namespace and psr4-path is a path (as a string), relative to
the project root (e.g. \"src/\"). Also see `php-helpers-default-psr4-autoloads'.

If match is found, return a pair `(psr4-ns-str . psr4-path)'. The meaning is the
same as described above, but psr4-ns-str is a string rather than symbol.

If no match is found, return (nil . nil).
Note: `nil' is neutral with respect to `concat', yet it is not `stringp'."
  (let ((default-result (cons nil nil)))
    (cl-dolist (item psr4-configs default-result)
      (cl-destructuring-bind (psr4-ns . psr4-path) item
        (when (s-prefix? psr4-path file-relpath)
          (return (cons (symbol-name psr4-ns) psr4-path)))))))

(defun php-helpers/file-path->psr4-namespace (file-path)
  "Transform FILE-PATH into a PSR-4 compliant PHP namespace.
FILE-PATH must be a string. Result is a string as well."
  (s-with file-path
    (s-chop-prefix "/")
    (s-split "/")
    butlast
    (s-join "\\")))

(cl-defun php-helpers/infer-file-namespace (&optional (file-path buffer-file-name))
  "Infer namespace from a path of the current file.

Currently, the following heuristics are used:
1. Project root is stripped off. See `projectile-project-root'.
2. composer.json settings \"autoload\".\"psr-4\" and \"autoload-dev\".\"psr-4\"
   are processed. See `php-helpers/parse-composer-psr4-autoloads'.
3. Should p.2 fail, defaults from `php-helpers-default-psr4-autoloads'
   will also be tried, in order."
  (let* ((project-root (projectile-project-root (file-name-directory file-path)))
         (relative-path (s-chop-prefix project-root file-path))
         (psr4-configs (append
                        (php-helpers//parse-composer-psr4-config project-root)
                        php-helpers-default-psr4-autoloads))
         (psr4-config (php-helpers//find-matching-prs4-config relative-path
                                                              psr4-configs)))
    (cl-destructuring-bind (psr4-ns . psr4-path) psr4-config
      ;; If psr4-ns and psr4-path are nil, nothing extra is added/chopped.
      (concat psr4-ns
              (php-helpers/file-path->psr4-namespace (s-chop-prefix
                                                      psr4-path
                                                      relative-path))))))

;;;###autoload
(defun php-helpers/insert-inferred-file-namespace (file-path)
  "Insert inferred namespace at point for FILE-PATH.

If called interactively, use the current buffer's file path.

See `php-helpers/infer-file-namespace' on how inference is done."
  (interactive (list (buffer-file-name)))
  (insert (php-helpers/infer-file-namespace file-path)))

;;;###autoload
(defun php-helpers/class-short-name-from-file-path (file-path)
  "Infer a short class name from the FILE-PATH.

This is done by stripping the directory part and the extension.
If called interactively, the result will also be inserted at point."
  (interactive (list (buffer-file-name)))
  (let ((class-name (f-base file-path)))
    (when (interactive-p)
      (insert class-name))
    class-name))

;;;###autoload
(defun php-helpers/insert-var-doc-comment ()
  "Insert doc comment for the variable at point, so we can explicitly specify its type."
  (interactive)
  (evil-emacs-state)
  (let ((var-at-point (symbol-at-point)))
    (end-of-line 0)
    (newline-and-indent)
    (insert "/** @var ")
    (let ((return-point (point)))
      (insert " $" (symbol-name var-at-point) " */")
      (set-window-point nil return-point)
      (evil-insert-state))))

(provide 'php-helpers)

;;; php-helpers.el ends here
