(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-self)

(when (>= emacs-major-version 24)
  (require 'init-melpa))
