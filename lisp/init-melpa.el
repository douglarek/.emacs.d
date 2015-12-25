;;; init-melpa.el --- Melpa init file.

;;; Commentary:

;; This file is used to manage melpa packages' settings.

;;; Code:

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Refresh the packages descriptions
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to load
(setq package-load-list '(all))

;; Install all needed
(defvar my-packages '(ace-jump-buffer
		      ace-jump-mode
		      bing-dict
		      circe
		      exec-path-from-shell
		      flycheck
		      flycheck-pos-tip
		      git-gutter
		      git-messenger
		      go-autocomplete
		      go-eldoc
		      golint
		      go-mode
		      gorepl-mode
		      ix
		      magit
		      move-dup
		      multiple-cursors
		      projectile
		      restclient
		      smartparens
		      smex
		      undo-tree
		      whole-line-or-region
		      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))


;; A quick cursor jump mode for emacs
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; Super fast Emacs buffer switching extension for ace-jump-mode
(global-set-key (kbd "M-g b") 'ace-jump-buffer)


;; Make Emacs use the $PATH set up by the user's shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))

;; Flycheck errors display in tooltip, only when display-graphic-p
(when (display-graphic-p)
  (eval-after-load 'flycheck
    '(custom-set-variables
	    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; git-gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(global-git-gutter-mode t)


;; Emacs Port of git-messenger.vim
(require 'git-messenger)
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)


;; An improved Go mode for emacs
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			   (local-set-key (kbd "C-c C-i") 'go-goto-imports)
			   (local-set-key (kbd "M-.") 'godef-jump) ; "M-*" back
			   (make-local-variable 'before-save-hook)
			   (setq gofmt-command "goimports")
			   (add-hook 'before-save-hook 'gofmt-before-save)
			   (local-set-key (kbd "C-c C-f") 'gofmt)
			   (when (executable-find "oracle")
			     (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))))

;; Go completion for Emacs
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map "\r" 'ac-complete)
(define-key ac-completing-map "\t" nil)

;; Eldoc for Go
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; A minor emacs mode for Go REPL
(add-hook 'go-mode-hook #'gorepl-mode)


;; Project Interaction Library for Emacs
(projectile-global-mode)
(setq projectile-enable-caching t)


;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(smartparens-global-mode t)


;; smex -> A smart M-x enhancement for Emacs.
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Treat undo history as a tree
(global-undo-tree-mode t)


;; Operate on current line if region undefined
(whole-line-or-region-mode t)


;; Yasnippet -> A template system for Emacs
(defun joindirs (root &rest dirs)
  "Join dirs ROOT DIRS."
  (if (not dirs)
      root
    (apply 'joindirs
	   (expand-file-name (car dirs) root)
	   (cdr dirs))))

(defvar yasnippet-snippets (joindirs user-emacs-directory "snippets" "yasnippet-snippets"))
(defvar go-snippets (joindirs user-emacs-directory "snippets" "go-snippets"))
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))
(setq yas-snippet-dirs '(yasnippet-snippets my-snippets go-snippets))

(require 'yasnippet)
(yas-reload-all)
(dolist (hook '(go-mode-hook))
  (add-hook hook #'yas-minor-mode))

;; Circe, a Client for IRC in Emacs
(setq circe-network-options
      `(("Freenode")))

(setq circe-reduce-lurker-spam t)

(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  "My circe prompt."
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
		       'face 'circe-prompt-face)
	   " ")))

(eval-after-load 'circe '(require 'circe-chanop))

(setq lui-time-stamp-position 'right-margin
      lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  "My lui setup."
  (setq fringes-outside-margins t
	right-margin-width 10
	word-wrap t
	wrap-prefix "    "))

(provide 'init-melpa)
;;; init-melpa.el ends here
