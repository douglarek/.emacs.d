;;; init.el --- Emacs startup file

;;; Commentary:

;; This file is used to control multiple third-party init files.

;;; Code:

;(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (>= emacs-major-version 24)
  (require 'init-self)
  (require 'init-melpa))

(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(setq custom-file custom-file)
(load custom-file)

;;; init.el ends here
