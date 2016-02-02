;;; amd-mode.el --- Minor mode for handling JavaScript AMD module requirements.

;; Copyright (C) 2014-2015  Nicolas Petton
;;
;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;; Keywords: javascript, amd, projectile
;; Version: 1.0
;; Package: amd-mode
;; Package-Requires: ((projectile "0.10.0") (s "1.9.0") (f "0.16.2") (dash "2.5.0") (makey "0.3") (js2-mode "20140114") (js2-refactor "0.6.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; ;;; Commentary:
;; amd-mode.el provides convenience methods and keybindings for handling
;; AMD module definitions.
;;
;; amd-mode.el works with js2-mode and (at the moment) requires to be
;; with a projectile project.
;;
;; C-c C-d k: `amd-kill-buffer-module': Kill the module path of the
;; buffer's file.
;;
;; C-c C-d s: `amd-search-references': Search for modules that require
;; the buffer's file.
;;
;; C-c C-d f: `amd-import-file': Prompt for a file to import. When
;; called with a prefix argument, always insert the relative path of
;; the file.

;; C-c C-d m: `amd-import-module': Prompt for a module name to
;; import.
;;
;; C-c C-d o: `amd-find-module-at-point': Find a module named after
;; the node at point.
;;
;; C-c C-d a: `amd-auto-insert': Insert an empty module definition.
;;
;; C-S-up: reorder the imported modules or perform
;; `js2r-move-line-up`.
;;
;; C-S-down: reorder the imported modules or perform
;; `js2r-move-line-down`.
;;
;; When `amd-use-relative-file-name' is set to `T', files are
;; imported using relative paths when the imported file is in a
;; subdirectory or in the same directory as the current buffer
;; file.
;;
;; When `amd-always-use-relative-file-name' is set to `T', files are
;; always imported using relative paths.

;;; Code:

(require 'js2-mode)
(require 'js2-refactor)
(require 'projectile)
(require 'makey)
(require 's)
(require 'f)
(require 'dash)

(defcustom amd-use-relative-file-name nil
  "Use relative file names for new module imports.

Relative file names are only used if the module is in a
subdirectory or in the same directory as the current buffer
file."
  :group 'amd-mode
  :type 'boolean)

(defcustom amd-always-use-relative-file-name nil
  "Use relative file names for new module imports.

Relative file names are always used."
  :group 'amd-mode
  :type 'boolean)

(defcustom amd-write-file-function 'write-file
  "Function used to write files."
  :group 'amd-mode
  :type 'symbol)

(defvar amd-rewrite-rules-alist '()
  "When importing a file, apply each rule against the file path.
It has no effect on inserting module names not corresponding to files.

It can be convenient to set `amd-rewrite-rules-alist' as a
directory-local variable in the root of a project.

Example:
(setq amd-rewrite-rules-alist '((\"^foo/\" . \"\")))

Importing the file \"foo/bar/baz.js\" will result in inserting
\"bar/baz\" as the module path.")

(make-local-variable 'amd-rewrite-rules-alist)

(defvar amd-mode-map
  (make-sparse-keymap)
  "Keymap for amd-mode.")

(define-minor-mode amd-mode
  "Minor mode for handling AMD modules within a JavaScript file."
  :lighter " AMD"
  :keymap amd-mode-map)

(defun amd-kill-buffer-module ()
  "Kill the module path of the buffer's file.
The path is relative to the current projectile project root
directory."
  (interactive)
  (amd--guard)
  (kill-new (concat "'"
                    (amd--buffer-module)
                    "'")))

(defun amd-search-references ()
  "Find amd references of the buffer's module in the current project."
  (interactive)
  (amd--guard)
  (projectile-ag (amd--file-search-regexp) t))

(defun amd-find-module-at-point ()
  "When on a node, find the module file at point represented by the content of the node."
  (interactive)
  (amd--guard)
  (let* ((current-node (js2-node-at-point))
         (string-contents (amd--node-content current-node))
         (name (s-replace-all '(("'" . "") ("\"" "")) string-contents)))
    (amd--find-file-matching name)))

(defun amd-auto-insert ()
  "Auto insert a default template contents for AMD files."
  (interactive)
  (amd--guard)
  (goto-char (point-min))
  (insert "define(function(){

});")
  (backward-char 3)
  (js2-indent-line))

(defun amd-import-file ()
  "Prompt for a file and insert it as a dependency.
Also appends the filename to the modules list."
  (interactive)
  (amd--guard)
  (save-excursion
    (let ((file (projectile-completing-read
                 "Import file: "
                 (projectile-current-project-files)
                 (word-at-point))))
      (amd--import file))))

(defun amd-rename-file ()
  "Rename the current buffer file and update references."
  (interactive)
  (amd--guard)
  (let* ((original-file (buffer-file-name))
         (original-file-name  (file-name-nondirectory
                               (file-name-sans-extension
                                original-file)))
         (file-replace-regexp (amd--file-replace-regexp))
         (files (-remove-item original-file
                              (mapcar #'projectile-expand-root
                                      (projectile-files-with-string original-file-name
                                                                    (projectile-project-root))))))
    (call-interactively amd-write-file-function)
    (delete-file original-file)
    (message "Renaming references in project...")
    (amd--replace-all-file-references file-replace-regexp
                                   (current-buffer)
                                   files
                                   #'amd--inside-imports-p)
    (when (y-or-n-p "Save all project buffers? ")
      (projectile-save-project-buffers))))

(defun amd--replace-all-file-references (from buffer files &optional condition)
  "Replace FROM with the AMD name of BUFFER in FILES, if CONDITION returns non-nil.
Replacement is only done in JS files, other files are ignored.
TO can be a string or a function returning a string."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
     (when (string= "js" (file-name-extension file))
       (condition-case error
           (amd--replace-references-in-file from
                                            `(lambda ()
                                               (format "%s" (amd--module (amd--buffer-file-name ,buffer))))
                                            file
                                            condition)
         (error (message "Unable to perform replacement in %s" file)))))))

(defun amd--replace-references-in-file (from to file &optional condition)
  (unless (stringp to)
    (setq to (funcall to)))
  (unless condition
    (setq condition (lambda () t)))
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and
                (re-search-forward from nil t)
                (funcall condition)
                (replace-match (concat "\\1" to "\\3"))))))))

(defun amd-import-module (module)
  "Prompt for MODULE and insert it as a dependency.
Also append it to the modules list."
  (interactive (list (read-string "Import module name: " (word-at-point))))
  (save-excursion
    (amd--guard)
    (amd--import module)))

(defun amd-move-line-up ()
  "When inside the import array, move up the module at point.
Always perform `js2r-move-line-up'."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (when (amd--inside-imports-p)
      (amd--move-module-up)))
  (js2r-move-line-up))

(defun amd-move-line-down ()
  "When inside the import array, move down the module at point.
Always perform `js2r-move-line-down'."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (when (amd--inside-imports-p)
      (amd--move-module-down)))
  (js2r-move-line-down))

(defun amd--guard ()
  "Throw an error when not in a projectile project."
  (unless (projectile-project-p)
    (error "Not within a project")))

(defun amd--move-module-up ()
  (amd--move-module -1))

(defun amd--move-module-down ()
  (amd--move-module 1))

(defun amd--move-module (offset)
  (let* ((current-node (js2-node-at-point))
         (function-node (amd--define-function-node))
         (params (amd--function-node-params function-node))
         (names (amd--function-node-params function-node))
         (position (js2-position current-node
                                 (js2-array-node-elems (js2-node-parent current-node))))
         (module-to-move (nth position names)))
    (setf (nth position names) (nth (+ offset position) names))
    (setf (nth (+ offset position) names) module-to-move)
    (amd--set-function-params function-node names)))

(defun amd--delete-function-params (node)
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (let ((beg (search-forward "("))
          (end (- (search-forward ")") 1)))
      (delete-region beg end))))

(defun amd--set-function-params (node params)
  (amd--delete-function-params node)
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (search-forward "(")
    (insert (car params))
    (dolist (param (cdr params))
      (insert ", ")
      (insert param))))

(defun amd--define-function-node ()
  (save-excursion
    (js2-node-at-point (amd--goto-define-function))))

(defun amd--import (file-or-name)
  "Insert FILE-OR-NAME as a AMD module dependency. Also append it
 to the modules list."
  (amd--insert-module-name file-or-name)
  (amd--insert-dependency file-or-name))

(defun amd--insert-dependency (file-or-name)
  "Insert FILE-OR-NAME as a dependency in the imports array."
  (amd--goto-imports)
  (insert (concat "'"
                  (amd--module file-or-name)
                  "'"))
  (js2-indent-line))

(defun amd--insert-module-name (file)
  (let* ((default-module-name (amd--module-name file))
         (module-name (read-string (concat "Import as ("
                                           default-module-name
                                           "): "))))
    (amd--goto-define-function-params)
    (search-forward ")")
    (backward-char 1)
    (unless (looking-back "(" nil)
      (insert ", "))
    (insert (if (string= "" module-name)
                default-module-name
              module-name))))

(defun amd--module-name (file)
  (file-name-nondirectory
   (file-name-sans-extension file)))

(defun amd--goto-define ()
  (goto-char (point-min))
  (search-forward "define(")
  (skip-chars-forward " \n\t"))

(defun amd--goto-define-function-params ()
  (amd--goto-define)
  (search-forward "("))

(defun amd--goto-define-function ()
  (amd--goto-define-function-params)
  (search-backward "function"))

(defun amd--goto-imports ()
  (amd--goto-define)
  (let* ((current-node (js2-node-at-point))
         (last-child (js2-node-last-child current-node)))
    (if (js2-array-node-p current-node)
        (if last-child
            (progn
              (goto-char (js2-node-abs-end last-child))
              (insert ",\n"))
          (forward-char 1))
      (progn
        (insert "[\n\n], ")
        (forward-char -4))))
  (js2-indent-line))

(defun amd--buffer-file-name (&optional buffer)
  "Return the name of the buffer's file relative to the current
project."
  (amd--file-name (buffer-file-name buffer)))

(defun amd--buffer-module ()
  (amd--module (buffer-file-name)))

(defun amd--node-content (node)
  (let* ((beg (js2-node-abs-pos node))
         (end (+ beg (js2-node-len node))))
    (buffer-substring beg end)))

;; Using `js2-function-node-params' does not always work when the file
;; has not been fully parsed yet.
(defun amd--function-node-params (node)
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (let ((beg (search-forward "("))
          (end (- (search-forward ")") 1)))
      (mapcar #'s-trim
              (split-string (buffer-substring beg end) ",")))))

(defun amd--find-file-matching (name)
  "Prompt for a file matching NAME in the project.

Note: This function is mostly a copy/paste from
`projectile-find-file`"
  (let* ((matching-files (amd--current-files-matching name))
         (file (projectile-completing-read "Find file: " matching-files)))
    (find-file (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

(defun amd--current-files-matching (name)
  (-filter #'(lambda (file)
               (s-contains? name file))
           (projectile-current-project-files)))

(defun amd--file-name (file)
  "Return the name of FILE relative to the project or the current
buffer file."
  (if (amd--use-relative-file-name-p file)
      (amd--relative-file-name file)
    (amd--project-file-name file)))

(defun amd--relative-file-name (file)
  "Return the name of FILE relative to the current buffer file."
  (let ((relative-path (f-relative file (amd--buffer-directory))))
    (if (s-prefix-p "." relative-path)
        relative-path
      (concat "./" relative-path))))

(defun amd--project-file-name (file)
  "Return the name of FILE relative to the project."
  (file-relative-name file (projectile-project-root)))

(defun amd--module (file-or-name)
  "Return the module path for FILE-OR-NAME"
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p file-or-name)
        (amd--rewrite-path (file-name-sans-extension (amd--file-name file-or-name)))
      file-or-name)))

(defun amd--rewrite-path (path)
  "Rewrite PATH according to `amd-rewrite-rules-alist'."
  (dolist (rule amd-rewrite-rules-alist)
    (setq path (replace-regexp-in-string (car rule) (cdr rule) path)))
  path)

(defun amd--buffer-directory ()
  (file-name-directory (amd--buffer-file-name)))

(defun amd--use-relative-file-name-p (file)
  "Return non-nil if the relative file name of FILE should be used."
  (if (string= file (buffer-file-name))
      nil
    (or amd-always-use-relative-file-name
        (and amd-use-relative-file-name
             (s-prefix-p (amd--buffer-directory)
                         file)))))

(defun amd--inside-imports-p ()
  (or (amd--imports-node-p (js2-node-at-point))
      (amd--imports-node-p (js2-node-parent (js2-node-at-point)))))

(defun amd--imports-node-p (node)
  (let* ((imports-node node)
         (define-node (js2-node-parent imports-node)))
    (and (js2-array-node-p imports-node)
         (amd--define-node-p define-node))))

(defun amd--define-node-p (node)
  (let ((target (js2-call-node-target node)))
    (and (js2-call-node-p node)
         (string= (js2-name-node-name target) "define"))))

(defun amd--file-search-regexp ()
  (concat
   "\[\'|\"\].*"
   (file-name-nondirectory
    (file-name-sans-extension
     (buffer-file-name)))
   "\[\'|\"\]"))

(defun amd--file-replace-regexp ()
  (concat
   "\\(define([^)]+['|\"]\\)\\(.*"
   (file-name-nondirectory
    (file-name-sans-extension
     (buffer-file-name)))
   "\\)\\(['|\"]\\)"))

(defun amd-initialize-makey-group ()
  (interactive)
  (makey-initialize-key-groups
   '((amd
      (description "AMD module helpers")
      (lisp-switches
       ("-r" "Import using relative paths (files only)" amd-always-use-relative-file-name t nil))
      (actions
       ("Dependencies"
        ("k" "Kill buffer module" amd-kill-buffer-module)
        ("f" "Import file" amd-import-file)
        ("m" "Import module name" amd-import-module)
        ("r" "Rename file and update dependencies" amd-rename-file))
       ("Search"
        ("o" "Find module at point" amd-find-module-at-point)
        ("s" "Search references" amd-search-references))
       ("Auto insert"
        ("a" "Auto insert" amd-auto-insert))))))
  (makey-key-mode-popup-amd))

(define-key amd-mode-map (kbd "C-c C-a") #'amd-initialize-makey-group)
(define-key amd-mode-map (kbd "<C-S-up>") #'amd-move-line-up)
(define-key amd-mode-map (kbd "<C-S-down>") #'amd-move-line-down)



(provide 'amd-mode)
;;; amd-mode.el ends here
