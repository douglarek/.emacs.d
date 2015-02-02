;;; init.el --- Emacs startup file

;;; Commentary:

;; This file is used to control multiple third-party init files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (>= emacs-major-version 24)
  (require 'init-self)
  (require 'init-melpa))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
