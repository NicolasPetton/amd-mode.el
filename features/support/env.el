(require 'f)

(defvar amd-mode-support-path
  (f-dirname load-file-name))

(defvar amd-mode-features-path
  (f-parent amd-mode-support-path))

(defvar amd-mode-root-path
  (f-parent amd-mode-features-path))

(add-to-list 'load-path amd-mode-root-path)

(defvar test-base-directory default-directory)
(setq temporary-file-directory
      (expand-file-name "test-files" test-base-directory))

(require 'amd-mode)
(require 'js2-mode)
(require 'espuds)
(require 'ert)

(Setup
)

(Before
 (cd test-base-directory))

(After
 ;; After each scenario is run
 (mapc #'delete-file
       (directory-files temporary-file-directory t "^bar\.js.+")))

(Teardown
 ;; After when everything has been run
 )
