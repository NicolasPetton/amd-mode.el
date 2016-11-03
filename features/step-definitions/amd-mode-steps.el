;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(eval-when-compile (require 'js2-mode))

(Given "^\\(?:I am in file\\|I visit the file\\) \"\\(.+\\)\"$"
  "Visits FILE."
  (lambda (file)
    (find-file file)))

(And "^I wait for parsing to finish$"
  (lambda ()
    (js2-parse)))
