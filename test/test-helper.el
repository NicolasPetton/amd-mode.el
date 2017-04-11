;;; test-helper.el --- Helpers to write amd-mode tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; This code should be removed as soon as
;; https://github.com/phillord/assess/pull/7 is merged and assess is
;; released.
(unless (fboundp 'assess-with-filesystem)
  (defun assess-with-filesystem--make-parent (spec path)
    "If SPEC is a file name, create its parent directory rooted at PATH."
    (save-match-data
      (when (string-match "\\(.*\\)/" spec)
        (make-directory (concat path "/" (match-string 1 spec)) t))))

  (defun assess-with-filesystem--init (spec &optional path)
    "Interpret the SPEC inside PATH."
    (setq path (or path "."))
    (cond
     ((listp spec)
      (cond
       ;; non-empty file
       ((and (stringp (car spec))
             (stringp (cadr spec)))
        (when (string-match-p "/\\'" (car spec))
          (error "Invalid syntax: `%s' - cannot create a directory with text content" (car spec)))
        (assess-with-filesystem--make-parent (car spec) path)
        (with-temp-file (concat path "/" (car spec))
          (insert (cadr spec))))
       ;; directory
       ((and (stringp (car spec))
             (consp (cadr spec)))
        (make-directory (concat path "/" (car spec)) t)
        (mapc (lambda (s) (assess-with-filesystem--init
                      s (concat path "/" (car spec)))) (cadr spec)))
       ;; recursive spec, this should probably never happen
       (t (mapc (lambda (s) (assess-with-filesystem--init s path)) spec))))
     ;; directory specified using a string
     ((and (stringp spec)
           (string-match-p "/\\'" spec))
      (make-directory (concat path "/" spec) t))
     ;; empty file
     ((stringp spec)
      (assess-with-filesystem--make-parent spec path)
      (write-region "" nil (concat path "/" spec) nil 'no-message))
     (t (error "Invalid syntax: `%s'" spec))))

  (defmacro assess-with-filesystem (spec &rest forms)
    "Create temporary file hierarchy according to SPEC and run FORMS.

SPEC is a list of specifications for file system entities which
are to be created.

File system entities are specified as follows:

1. a string FILE is the name of file to be created
  - if the string contains \"/\", parent directories are created
    automatically
  - if the string ends with \"/\", a directory is created
2. a list of two elements (FILE CONTENT) specifies filename and the
  content to put in the file
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a directory this way
3. a list where car is a string and cadr is a list (DIR SPEC) is a
  recursive specification evaluated with DIR as current directory
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a file this way, a directory is always created

An example showing all the possibilities:

  (\"empty_file\"
   \"dir/empty_file\"
   \"dir/subdir/\"
   (\"non_empty_file\" \"content\")
   (\"dir/anotherdir/non_empty_file\" \"tralala\")
   (\"big_dir\" (\"empty_file\"
              (\"non_empty_file\" \"content\")
              \"subdir/empty_file\")))

If we want to run some code in a directory with an empty file
\"foo.txt\" present, we call:

  (assess-with-filesystem '(\"foo\")
    (code-here)
    (and-some-more-forms))

You should *not* depend on where exactly the hierarchy is created.
By default, a new directory in `temporary-file-directory' is
created and the specification is evaluated there, but this is up
for change."
    (declare (indent 1))
    (let ((temp-root (make-symbol "temp-root"))
          (old-dd (make-symbol "old-dd")))
      `(let ((,temp-root (make-temp-file "temp-fs-" t))
             (,old-dd default-directory))
         (unwind-protect
             (progn
               (setq default-directory ,temp-root)
               (mapc (lambda (s) (assess-with-filesystem--init s ".")) ,spec)
               ,@forms)
           (delete-directory ,temp-root t)
           (setq default-directory ,old-dd))))))

;;; test-helper.el ends here
