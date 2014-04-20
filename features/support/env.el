(require 'f)

(defvar amd-mode-support-path
  (f-dirname load-file-name))

(defvar amd-mode-features-path
  (f-parent amd-mode-support-path))

(defvar amd-mode-root-path
  (f-parent amd-mode-features-path))

(add-to-list 'load-path amd-mode-root-path)

(require 'amd-mode)
(require 'js2-mode)
(require 'espuds)
(require 'ert)

(Setup
)

(Before
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
