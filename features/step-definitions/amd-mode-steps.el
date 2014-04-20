;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^\\(?:I am in file\\|I visit to file\\) \"\\(.+\\)\"$"
  "Visits FILE."
  (lambda (file)
    (find-file file)))
