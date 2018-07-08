;;; init.el --- Emacs startup file

;;; Commentary:

;;; Code:

;; (package-initialize)

;; Emacs 25+ custom file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(setq custom-file custom-file)
(load custom-file)

;; Built-in init
;; Backup settings
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

;; Auto-save settings
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

;; Disable startup message
(setq inhibit-startup-message t)

;; Save place in files between sessions
(save-place-mode 1)

;; Enable ido
(ido-mode t)
; Disable the merging (the "looking in other directories" in ido vulgo), or switch with 'C-z' manually
(setq ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t)

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook #'bind-ido-keys)

;; Turn on column numbers
(setq column-number-mode t)

;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saved-places"))
(setq-default save-place t)

;; Turn on upcase region
(put 'upcase-region 'disabled nil)

;; Turn on show-paren-mode
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
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-default-font "Courier 13"))

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


;; Start maximized frame
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Emacs dark theme title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Remove title bar file icon.
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)


;; Melpa init

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://elpa.emacs-china.org/melpa/") t)
(package-initialize)

;; Refresh and install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; A use-package declaration for simplifying your .emacs
(setq use-package-verbose t
      use-package-always-ensure t)
(eval-when-compile (require 'use-package))
(require 'bind-key)

;; It's Magit! A Git Porcelain inside Emacs. https://magit.vc
(use-package magit)

;; Minor mode for Eclipse-like moving and duplicating lines or rectangles.
(use-package move-dup)

;; Multiple cursors for emacs.
(use-package multiple-cursors)

;; A minor mode for performing structured editing of S-expression data
(use-package paredit :defer t :diminish paredit-mode)

;; Easily (I hope) search and replace multiple variants of a word
(use-package plur)

;; HTTP REST client tool for emacs
(use-package restclient)

;; Swapping two regions of text in Emacs
(use-package swap-regions)

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-;" . avy-goto-char)
	 ("M-g l" . avy-goto-line)))

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-check-startup-files nil)
  :when (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (let ((envs '("GOROOT" "GOPATH" "PYTHONPATH")))
    (exec-path-from-shell-copy-envs envs)))


;; An Intelligent auto-completion extension for Emacs
(use-package auto-complete
  :diminish auto-complete-mode
  :init (auto-complete-mode t)
  :bind (:map ac-completing-map
	      ("C-p" . ac-previous)
	      ("C-n" . ac-next)
	      ("\r" . ac-complete)
	      ("\t" . nil))
  :config
  (use-package ac-emoji)
  (ac-config-default))

;; Modular in-buffer completion framework for Emacs
(use-package company
  :defer t
  :diminish company-mode
  :bind (:map company-active-map
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next))
  :config
  (setq company-minimum-prefix-length 2)
  (use-package company-flx :diminish)
  (with-eval-after-load 'company
    (company-flx-mode +1)))

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(save)))

;; Git-gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-x v s" . git-gutter:stage-hunk)
	 ("C-x v r" . git-gutter:revert-hunk))
  :init (global-git-gutter-mode t))

;; Ido/helm imenu tag selection across all buffers with the same mode
(use-package imenu-anywhere
  :bind ("C-c C-j" . imenu-anywhere))

;; Emacs Port of git-messenger.vim
(use-package git-messenger
  :bind (("C-x v p" . git-messenger:popup-message)
	 :map git-messenger-map
	 ("m" . git-messenger:copy-message))
  :init (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  :config (use-package magit))

;; An improved Go mode for emacs
(use-package go-mode
  :defer t
  :config
  (use-package go-impl)
  (use-package golint)
  (use-package go-guru)
  (use-package go-rename)
  (use-package go-autocomplete)
  (use-package auto-complete)
  (use-package go-add-tags)
  (use-package go-fill-struct)
  (use-package go-gen-test)
  (use-package go-tag :config (setq go-tag-args (list "-transform" "camelcase")))
  (use-package godoctor)
  (defun my-go-mode()
    (local-set-key (kbd "M-.") 'godef-jump)
    (make-local-variable 'before-save-hook)
    (setq gofmt-command "goimports")
    (setq flycheck-disabled-checkers '(go-errcheck))
    (add-hook 'before-save-hook 'gofmt-before-save)
    (define-key go-mode-map (kbd "C-c C-j") nil)
    (yas-minor-mode)
    (with-eval-after-load 'go-mode
      (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))
  (add-hook 'go-mode-hook 'my-go-mode))
(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Project Interaction Library for Emacs
(use-package projectile
  :config (projectile-global-mode))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(use-package smartparens
  :diminish smartparens-mode
  :config (smartparens-global-mode t))

;; Smex is a smart M-x enhancement for Emacs.
(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ;; This is your old M-x.
	 ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode t))

;; Operate on current line if region undefined
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config (whole-line-or-region-mode t))

;; Yasnippet is a template system for Emacs
(defun joindirs (root &rest dirs)
  "Join dirs ROOT DIRS."
  (if (not dirs)
      root
    (apply 'joindirs
	   (expand-file-name (car dirs) root)
	   (cdr dirs))))

;; Customized yasnippets
(defvar yasnippet-snippets (joindirs user-emacs-directory "snippets" "yasnippet-snippets"))
(defvar go-snippets (joindirs user-emacs-directory "snippets" "go-snippets" "snippets"))
(defvar rust-snippets (joindirs user-emacs-directory "snippets" "rust-snippets" "snippets"))
(defvar clojure-snippets (joindirs user-emacs-directory "snippets" "clojure-snippets"))
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (add-hook 'python-mode-hook #'yas-minor-mode)
  :config
  (setq yas-snippet-dirs '(yasnippet-snippets go-snippets rust-snippets clojure-snippets my-snippets))
  (yas-reload-all))

;; Emacs isearch with an overview. Oh, man!
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package swiper
  :bind ("C-s" . swiper)
  :config (advice-add 'swiper :after '(lambda () (recenter))))
(use-package counsel
  :bind (("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)))

;; Web-mode.el is an autonomous emacs major-mode for editing web templates
(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (defun my-web-mode()
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-ac-sources-alist
	  '(("css" . (ac-source-css-property))
	    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    (local-set-key (kbd "RET") 'newline-and-indent)
    (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match))
  (add-hook 'web-mode-hook 'my-web-mode))

;; A face dedicated to lisp parentheses
(use-package paren-face
  :config (global-paren-face-mode))

;; Emacs powerline and one-dark theme
(use-package spacemacs-common
  :ensure spacemacs-theme
  :when (display-graphic-p)
  :config
  (load-theme 'spacemacs-dark t))

;; Emacs OCaml mode
(use-package tuareg
  :defer t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'tuareg-mode-hook #'yas-minor-mode)
  (use-package merlin)
  (use-package ocp-indent)
  (use-package utop))
(use-package merlin
  :defer t
  :bind (:map merlin-mode-map
	      ("M-." . merlin-locate)
	      ("M-," . merlin-pop-stack))
  :config
  (setq merlin-ac-setup t)
  (add-hook 'caml-mode-hook 'merlin-mode))
(use-package utop
  :defer t
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))

;; Emacs support for the Clojure(Script) programming language
(use-package clojure-mode
  :defer t
  :diminish eldoc-mode
  :config
  (use-package paredit)
  (use-package rainbow-delimiters)
  (use-package aggressive-indent)
  (use-package inf-clojure)
  (use-package clj-refactor :diminish clj-refactor-mode)
  (use-package yasnippet)
  (use-package cider)
  (use-package auto-complete)
  (use-package company)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (defun my-clojure-mode()
    (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
    (subword-mode)
    (paredit-mode)
    (rainbow-delimiters-mode)
    (aggressive-indent-mode)
    (inf-clojure-minor-mode)
    (eldoc-mode)
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (cljr-add-keybindings-with-prefix "C-c C-m")
    (company-mode))
  (add-hook 'clojure-mode-hook 'my-clojure-mode)
  (add-hook 'inf-clojure-mode-hook #'eldoc-mode))

;; Python auto-completion for Emacs
(use-package jedi
  :defer t
  :bind (:map python-mode-map
	      ("M-." . jedi:goto-definition)
	      ("M-," . jedi:goto-definition-pop-marker)
	      ("C-c d" . jedi:show-doc))
  :init (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t)
  (setq flycheck-flake8-maximum-line-length 120))
(use-package py-autopep8
  :defer t
  :init (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  :config
  (setenv "PYTHONIOENCODING" "utf8")
  (setq py-autopep8-options '("--max-line-length=120")))

;; Youdao Dictionary interface for Emacs
(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point)
  :config (setq url-automatic-caching t))

;; Emacs package that displays available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;; Yet another Emacs paste mode, this one for Gist
(use-package gist :defer t)

;; Spacemacs powerline
(use-package spaceline
  :when (display-graphic-p)
  :config (spaceline-emacs-theme))

;;; init.el ends here
