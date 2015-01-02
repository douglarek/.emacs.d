;;; init-self.el --- Self customed init file

;;; Commentary:

;; This file is used to manage self customed settings.

;;; Code:

;; backup settings
(defvar backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p backup-directory))
        (make-directory backup-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; auto-save settings
(defvar auto-save-directory (concat user-emacs-directory "auto-save"))
(if (not (file-exists-p auto-save-directory))
        (make-directory auto-save-directory t))

(add-to-list 'auto-save-file-name-transforms
             (list "\\(.+/\\)*\\(.*?\\)" (expand-file-name "\\2" auto-save-directory))
             t)

;; If you would prefer to have an empty response (hitting `RET`) or
;; any other input to be `no` and still require the full word `yes` to use this function as an alias for `yes-or-no-p`
(defun my-yes-or-mumble-p (prompt)
   "PROMPT user with a yes-or-no question, but only test for yes."
   (if (string= "yes"
                (downcase
                 (read-from-minibuffer
                  (concat prompt "(yes or no) "))))
       t
     nil))

(defalias 'yes-or-no-p 'my-yes-or-mumble-p)

;; disable startup message
(setq inhibit-startup-message t)

;; disable menu bar
(menu-bar-mode -1)

;; enable ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; turn on column numbers
(setq column-number-mode t)

;; enable org indent on startup
(setq org-startup-indented t)

;; `run-python` use `ipython` instead `python` as much as possible
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(provide 'init-self)
;;; init-self.el ends here
