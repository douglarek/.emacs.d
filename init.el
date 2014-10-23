;;;;;;;;;;;;;;;;;;;;;;;; Self Settings ;;;;;;;;;;;;;;;;;;;;;;;;
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

;; yes -> y, no -> n
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

;;;;;;;;;;;;;;;;;;;;;;;; Package Installed ;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Refresh the packages descriptions
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to load
(setq package-load-list '(all))

;; Jedi -> Python auto-completion for Emacs.
(unless (package-installed-p 'jedi)
  (package-install 'jedi))
(add-hook 'python-mode-hook 'jedi:setup)
(unless (package-installed-p 'ein)
  (package-install 'ein))
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq jedi:complete-on-dot t)

;; Smex -> a smart M-x enhancement for Emacs.
(unless (package-installed-p 'semx)
  (package-install 'smex))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Whitespace-cleanup -> In Emacs, intelligently call whitespace-cleanup on save
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))

;; Git-Gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(global-git-gutter-mode t)
