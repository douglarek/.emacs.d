;;; init.el --- Emacs startup file

;;; Commentary:

;; This file is used to control multiple third-party init files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-self)

(when (>= emacs-major-version 24)
  (require 'init-melpa))

;;; init.el ends here
