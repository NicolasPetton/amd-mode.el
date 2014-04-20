;;; amd-mode.el --- Convenience minor mode for handling AMD module requirements.

;; Copyright (C) 2014  Nicolas Petton

;; Author: Nicolas Petton(require 'projectile) <petton.nicolas@gmail.com>
;; Keywords: javascript, amd
;; Version: 0.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;; amd-mode provides convenience methods and keybindings for handling
;; AMD module definitions.
;; 
;; C-c C-d k: `amd-kill-buffer-path': Kill the path of the buffer's
;; file without its extension.
;;
;; C-c C-d s: `amd-search-references': Search for modules that require
;; the buffer's file.
;;
;; C-c C-d a: `amd-add-dependency': Prompt for a file to add as a
;; dependency.
;;
;; C-c C-d o: `amd-find-module-at-point': Find a module named after
;; the node at point.
;;
;; C-c C-d i: `amd-auto-insert': Insert an empty module definition.


;;; Code:

(require 'js2-mode)
(require 'projectile)
(require 'makey)
(require 's)
(require 'dash)

(defvar amd-mode-map 
  (make-sparse-keymap)
  "Keymap for amd-mode")

(define-minor-mode amd-mode 
  "Minor mode for handling AMD modules within a JavaScript file."
  :lighter " AMD"
  :keymap amd-mode-map)

(defun amd-kill-buffer-path ()
  "Kill the path of the buffer's file without its extension.
The path is relative to the current projectile project."
  (interactive)
  (amd--guard)
  (kill-new (concat "'"
                    (amd--buffer-file-name-sans-extension)
                    "'")))

(defun amd-search-references ()
  "Find amd references of the buffer's file in the current
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
  "When on a node, find file at point represented by the
content of the node."
  (interactive)
  (amd--guard)
  (let* ((current-node (js2-node-at-point))
         (string-contents (amd--node-contents current-node))
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

(defun amd-add-dependency ()
  "Prompt for a file and insert it as a dependency. Also appends
the filename to the modules list."
  (interactive)
  (amd--guard)
  (save-excursion
    (let ((file (projectile-completing-read 
                 "Add dependency: " 
                 (projectile-current-project-files))))
      (amd--add-file-dependency file))))


(defun amd--guard ()
  "Throw an error when not in a projectile project"
  (unless (projectile-project-p)
    (error "Not within a project")))

(defun amd--add-file-dependency (file)
  "Insert FILE as a AMD dependency. Also appends the file-name of
FILE to the modules list."
  (amd--insert-module-name file)
  (amd--insert-dependency file))

(defun amd--insert-dependency (file)
  (amd--goto-define-dependencies)
  (insert (concat "'"
                  (amd--file-path file)
                  "'"))
  (js2-indent-line))

(defun amd--insert-module-name (file)
  (let ((module-name (file-name-nondirectory 
                      (file-name-sans-extension file))))
    (amd--goto-define-function)
    (search-forward ")")
    (backward-char 1)
    (unless (looking-back "(")
      (insert ", "))      
    (insert module-name)))

(defun amd--goto-define ()
  (goto-char (point-min))
  (search-forward "define("))

(defun amd--goto-define-function ()
  (amd--goto-define)
  (search-forward "function("))

(defun amd--goto-define-dependencies ()
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

(defun amd--buffer-file-name-sans-extension ()
  (amd--file-path (buffer-file-name)))

(defun amd--node-contents (node)
  (let* ((beg (js2-node-abs-pos node))
         (end (+ beg (js2-node-len node))))
    (buffer-substring beg end)))

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
  "Return the name of FILE relative to the project"
  (s-chop-prefix (projectile-project-root)
                 file))

(defun amd--file-path (file)
  (file-name-sans-extension (amd--file-name file)))


(defun amd-initialize-makey-group () 
  (interactive)
  (makey-initialize-key-groups
   '((amd
	  (description "AMD module helpers")
	  (actions
	   ("Dependencies"
            ("k" "Kill buffer path" amd-kill-buffer-path)
            ("a" "Add dependency" amd-add-dependency))
           ("Search"
            ("o" "Find module at point" amd-find-module-at-point)
            ("s" "Search references" amd-search-references))
           ("Auto insert"
            ("i" "Auto insert" amd-auto-insert))))))
  (makey-key-mode-popup-amd))

(define-key amd-mode-map (kbd "C-c C-d") #'amd-initialize-makey-group)

(provide 'amd-mode)
;;; amd-mode.el ends here
