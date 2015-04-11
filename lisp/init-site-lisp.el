;;; init-site-lisp.el --- Self site-lisp init file.

;;; Commentary:

;; This file is used to manage 3rd party plugins not in Melpa.

;;; Code:

(eval-when-compile (require 'cl))
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
	   (default-directory my-lisp-dir))
      (progn
	(setq load-path
	      (append
	       (loop for dir in (directory-files my-lisp-dir)
		     unless (string-match "^\\." dir)
		     collecting (expand-file-name dir))
	       load-path)))))


;; Use yapf to beautify a Python buffer
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)


(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
