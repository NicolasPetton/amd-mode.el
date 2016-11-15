;;; test-amd-mode.el --- tests for amd-mode          -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'amd-mode)

(require 'assess)
(require 'test-helper)

(defun test-amd--prepare-buffer (&optional buffer)
  "Configure BUFFER for javascript and amd-mode.
If BUFFER is nil, use `current-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (amd-auto-insert)
    (js2-mode)
    (amd-mode)
    (js2-parse)))

(ert-deftest auto-insert-should-add-default-template-to-empty-buffer ()
  (with-temp-buffer
    (test-amd--prepare-buffer)
    (should (assess= (buffer-string) "define([], function() {\n    \n});\n"))))

(ert-deftest add-dependency-should-add-a-file-in-child-dir ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
    (assess-with-filesystem '("sub/foo.js"
                              "bar.js"
                              ".projectile")
      (assess-with-find-file "bar.js"
        (test-amd--prepare-buffer)
        (setq amd-use-relative-file-name nil)
        (amd--import "sub/foo.js")
        (should (string-match-p
                 (regexp-quote "define(['sub/foo'], function(foo) {\n")
                 (substring-no-properties (buffer-string))))))))

(ert-deftest add-dependency-should-add-a-file-in-parent-dir ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
    (assess-with-filesystem '("sub/foo.js"
                              "sub/subsub/bar.js"
                              ".projectile")
      (assess-with-find-file "sub/subsub/bar.js"
        (test-amd--prepare-buffer)
        (setq amd-use-relative-file-name nil)
        (amd--import "sub/foo.js")
        (should (string-match-p
                 (regexp-quote "define(['sub/foo'], function(foo) {\n")
                 (substring-no-properties (buffer-string))))))))

(ert-deftest add-dependency-should-add-a-module ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
    (assess-with-filesystem '(".projectile")
      (with-temp-buffer
        (test-amd--prepare-buffer)
        (amd--import "lib/foo")
        (should (string-match-p
                 (regexp-quote "define(['lib/foo'], function(foo) {\n")
                 (substring-no-properties (buffer-string))))))))

(ert-deftest can-kill-buffer-path ()
  (assess-with-filesystem '(".projectile" "sub/foo")
    (assess-with-find-file "sub/foo"
      (amd-kill-buffer-module)
      (should (assess= "'sub/foo'" (car kill-ring))))))

(ert-deftest find-module-at-point ()
  (cl-letf (((symbol-function 'projectile-completing-read)
             (lambda (&rest _) "target.js")))
    (assess-with-filesystem '(".projectile"
                              "source.js"
                              ("target.js" "I'm in target.js"))
      (assess-with-find-file "source.js"
        (insert "var target;")
        (backward-word)
        (amd-find-module-at-point)
        (should (assess= "I'm in target.js"
                         (substring-no-properties (buffer-string))))))))

(provide 'test-amd-mode)
;;; test-amd-mode.el ends here
