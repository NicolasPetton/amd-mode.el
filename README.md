# amd-mode.el [![MELPA](https://melpa.org/packages/amd-mode-badge.svg)](https://melpa.org/#/amd-mode)
  
  Minor mode for handling JavaScript AMD module definitions.
  
  Mirrored from [https://petton.fr/git/nico/amd-mode.el](https://petton.fr/git/nico/amd-mode.el)

## Usage

  amd-mode.el works with [js2-mode](https://github.com/mooz/js2-mode) and (at the moment) requires to be
  with a [projectile](https://github.com/bbatsov/projectile) project.
  
- C-c C-d k: `amd-kill-buffer-module`: Kill the module path of the
  buffer's file.
  
- C-c C-d s: `amd-search-references`: Search for modules that require
  the buffer's file.
  
- C-c C-d f: `amd-import-file`: Prompt for a file to import. When
  called with a prefix argument, always insert the relative path of
  the file.
  
- C-c C-d m: `amd-import-module`: Prompt for a module name to
  import.
  
- C-c C-d o: `amd-find-module-at-point`: Find a module named after
  the node at point.
  
- C-c C-d a: `amd-auto-insert`: Insert an empty module definition.
  
- C-S-up: reorder the imported modules or perform
  `js2r-move-line-up`.
  
- C-S-down: reorder the imported modules or perform
  `js2r-move-line-down`.
  
When `amd-use-relative-file-name` is set to `t`, files are
imported using relative paths when the imported file is in a
subdirectory or in the same directory as the current buffer
file.

When `amd-always-use-relative-file-name` is set to `t`, files are
always imported using relative paths.

---

Copyright (C) 2014-2016 Nicolas Petton
  
