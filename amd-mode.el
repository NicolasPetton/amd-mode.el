;;; amd-mode.el --- Minor mode for handling JavaScript AMD module requirements.

;; Copyright (C) 2014  Nicolas Petton
;;
;; Author: Nicolas Petton(require 'projectile) <petton.nicolas@gmail.com>
;; Keywords: javascript, amd, projectile
;; Version: 0.4
;; Package: gnome-calendar
;; Package-Requires: ((projectile "0.10.0") (s "1.9.0") (dash "2.5.0") (makey "0.3") (js2-mode "20140114") (js2-refactor "0.6.1"))

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
;; C-c C-d i: `amd-import': Prompt for a file to import.
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
;; When `amd-use-relative-file-name` is set to `T', modules are
;; imported using relative paths when the imported module is in a
;; subdirectory or in the same directory as the current buffer
;; file.


;;; Code:

(require 'js2-mode)
(require 'js2-refactor)
(require 'projectile)
(require 'makey)
(require 's)
(require 'dash)

(defcustom amd-use-relative-file-name nil 
  "Use relative file names for new module imports.

Relative file names are only used if the module is in a
subdirectory or in the same directory as the current buffer
file."
  :group 'amd-mode
  :type 'boolean)

(defvar amd-mode-map 
  (make-sparse-keymap)
  "Keymap for amd-mode")

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
  "Find amd references of the buffer's module in the current
project."
  (interactive)
  (amd--guard)
  (projectile-ack (concat 
                   "['|\"].*"
                   (file-name-nondirectory 
                    (file-name-sans-extension 
                     (buffer-file-name)))
                   "['|\"]")))

(defun amd-find-module-at-point ()
  "When on a node, find the module file at point represented by
the content of the node."
  (interactive)
  (amd--guard)
  (let* ((current-node (js2-node-at-point))
         (string-contents (amd--node-content current-node))
         (name (s-replace-all '(("'" . "") ("\"" "")) string-contents)))
    (amd--find-file-matching name)))

(defun amd-auto-insert ()
  "Auto inserts a default template contents for AMD files."
  (interactive)
  (amd--guard)
  (goto-char (point-min))
  (insert "define(function(){

});")
  (backward-char 3)
  (js2-indent-line))

(defun amd-import ()
  "Prompt for a file and insert it as a dependency. Also appends
the filename to the modules list."
  (interactive)
  (amd--guard)
  (save-excursion
    (let ((file (projectile-completing-read 
                 "Import: " 
                 (projectile-current-project-files))))
      (amd--import file))))

(defun amd-move-line-up ()
  (interactive)
  "When inside the import array, move up the module at point.
Always perform `js2r-move-line-up'."
  (js2r-move-line-up)
  (when (amd--inside-imports-p)
    (amd--move-module-up)))

(defun amd-move-line-down ()
  (interactive)
  "When inside the import array, move down the module at point.
Always perform `js2r-move-line-down'."
  (js2r-move-line-down)
  (if (amd--inside-imports-p)
      (amd--move-module-down)))

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
         (module-to-move (nth (- position offset) names)))
    (setf (nth (- position offset) names) (nth position names))
    (setf (nth position names) module-to-move)
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

(defun amd--import (file)
  "Insert FILE as a AMD module dependency. Also appends the module name of
FILE to the modules list."
  (amd--insert-module-name file)
  (amd--insert-dependency file))

(defun amd--insert-dependency (file)
  (amd--goto-imports)
  (insert (concat "'"
                  (amd--module file)
                  "'"))
  (js2-indent-line))

(defun amd--insert-module-name (file)
  (let ((module-name (file-name-nondirectory 
                      (file-name-sans-extension file))))
    (amd--goto-define-function-params)
    (search-forward ")")
    (backward-char 1)
    (unless (looking-back "(")
      (insert ", "))      
    (insert module-name)))

(defun amd--goto-define ()
  (goto-char (point-min))
  (search-forward "define("))

(defun amd--goto-define-function-params ()
  (amd--goto-define)
  (search-forward "function"))

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

(defun amd--buffer-file-name ()
  "Return the name of the buffer's file relative to the current
project."
  (amd--file-name (buffer-file-name)))

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
  (let* ((prefix (amd--buffer-directory)))
    (concat "./" (file-relative-name file prefix))))

(defun amd--project-file-name (file)
  "Return the name of FILE relative to the project."
  (file-relative-name file (projectile-project-root)))

(defun amd--module (file)
  "Return the module path for FILE."
  (file-name-sans-extension (amd--file-name file)))

(defun amd--buffer-directory ()
  (file-name-directory (amd--buffer-file-name)))

(defun amd--use-relative-file-name-p (file)
  "Return T if the relative file name of FILE should be used."
  (if (string= file (buffer-file-name))
      nil
    (and amd-use-relative-file-name
         (s-prefix-p (amd--buffer-directory)
                     file))))

(defun amd--inside-imports-p ()
  (amd--imports-node-p (js2-node-at-point)))

(defun amd--imports-node-p (node)
  (let* ((imports-node (js2-node-parent node))
         (define-node (js2-node-parent imports-node)))
    (and (js2-array-node-p imports-node)
         (amd--define-node-p define-node))))

(defun amd--define-node-p (node)
  (let ((target (js2-call-node-target node)))
   (and (js2-call-node-p node)
        (string= (js2-name-node-name target) "define"))))


(defun amd-initialize-makey-group () 
  (interactive)
  (makey-initialize-key-groups
   '((amd
	  (description "AMD module helpers")
	  (actions
	   ("Dependencies"
            ("k" "Kill buffer module" amd-kill-buffer-module)
            ("i" "Import module" amd-import))
           ("Search"
            ("o" "Find module at point" amd-find-module-at-point)
            ("s" "Search references" amd-search-references))
           ("Auto insert"
            ("a" "Auto insert" amd-auto-insert))))))
  (makey-key-mode-popup-amd))

(define-key amd-mode-map (kbd "C-c C-d") #'amd-initialize-makey-group)
(define-key amd-mode-map (kbd "<C-S-up>") #'amd-move-line-up)
(define-key amd-mode-map (kbd "<C-S-down>") #'amd-move-line-down)



(provide 'amd-mode)
;;; amd-mode.el ends here
