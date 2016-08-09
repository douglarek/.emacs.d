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
       t nil))

(defalias 'yes-or-no-p 'my-yes-or-mumble-p)

;; disable startup message
(setq inhibit-startup-message t)

;; disable menu bar
(menu-bar-mode -1)

;; enable ido
(ido-mode t)
; Disable the merging (the "looking in other directories" in ido vulgo), or switch with 'C-z' manually
(setq ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t)

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook #'bind-ido-keys)

;; turn on column numbers
(setq column-number-mode t)

;; when you visit a file, point goes to the last place where it was when you previously visited the same file
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saved-places"))
(setq-default save-place t)

;; Turn on upcase region
(put 'upcase-region 'disabled nil)

;; turn on show-paren-mode
(show-paren-mode t)

;; Whitespace cleanup before buffers are saved
(add-hook 'before-save-hook 'whitespace-cleanup)

;; HideShow hides balanced-expression code blocks and multi-line comment blocks
(defvar hs-modes '(python-mode-hook rust-mode-hook))
(dolist (m hs-modes) (add-hook m 'hs-minor-mode))


;; Turn on downcase region
(put 'downcase-region 'disabled nil)


;; Emacs GUI font settings
(when (and (display-graphic-p) (eq system-type 'darwin))
  (scroll-bar-mode -1)
  (set-frame-font "Courier 16"))

(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (if (file-exists-p filePath)
      (with-temp-buffer
	(insert-file-contents filePath)
	(split-string (buffer-string) "\n" t))
    nil))

(let ((ea (read-lines (concat user-emacs-directory ".erc-auth"))))
  (when (not (= (length ea) 0))
  (setq erc-nick (car ea))))


;; Using ThingAtPoint and the Existing C-s C-w
;; http://www.emacswiki.org/emacs/SearchAtPoint
(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in Isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
			     'isearch-yank-word-or-char
			     isearch-mode-map))

(add-hook 'isearch-mode-hook
	  (lambda ()
	    "Activate my customized Isearch word yank command."
	    (substitute-key-definition 'isearch-yank-word-or-char
				       'my-isearch-yank-word-or-char-from-beginning
				       isearch-mode-map)))


;; Prompt before closing Emacs
;; http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html.
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Exit canceled")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)


(provide 'init-self)
;;; init-self.el ends here
