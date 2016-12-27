;;; init-melpa.el --- Melpa init file.

;;; Commentary:

;; This file is used to manage melpa packages' settings.

;;; Code:

(require 'package)
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Refresh and install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; A use-package declaration for simplifying your .emacs
(use-package use-package
  :config (setq use-package-always-ensure t))


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
  :bind (("C-c SPC" . avy-goto-char)
	 ("M-g l" . avy-goto-line)))


;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
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


;; git-gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-x v s" . git-gutter:stage-hunk)
	 ("C-x v r" . git-gutter:revert-hunk))
  :init (global-git-gutter-mode t))


;; ido/helm imenu tag selection across all buffers with the same mode
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
  (add-hook 'go-mode-hook (lambda ()
			    (use-package go-impl)
			    (use-package golint)
			    (use-package go-guru)
			    (use-package go-rename)
			    (local-set-key (kbd "M-.") 'godef-jump) ; "M-*" back
			    (make-local-variable 'before-save-hook)
			    (setq gofmt-command "goimports")
			    (setq flycheck-disabled-checkers '(go-errcheck))
			    (add-hook 'before-save-hook 'gofmt-before-save)
			    (define-key go-mode-map (kbd "C-c C-j") nil)
			    (yas-minor-mode 1)
			    (go-eldoc-setup)))
  (use-package go-autocomplete)
  (use-package go-eldoc :diminish eldoc-mode))

(use-package go-autocomplete
  :defer t
  :config (use-package auto-complete))


;; Project Interaction Library for Emacs
(use-package projectile
  :config (projectile-global-mode))


;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(use-package smartparens
  :diminish smartparens-mode
  :config (smartparens-global-mode t))


;; smex -> A smart M-x enhancement for Emacs.
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


;; Yasnippet -> A template system for Emacs
(defun joindirs (root &rest dirs)
  "Join dirs ROOT DIRS."
  (if (not dirs)
      root
    (apply 'joindirs
	   (expand-file-name (car dirs) root)
	   (cdr dirs))))

(defvar yasnippet-snippets (joindirs user-emacs-directory "snippets" "yasnippet-snippets"))
(defvar go-snippets (joindirs user-emacs-directory "snippets" "go-snippets" "snippets"))
(defvar rust-snippets (joindirs user-emacs-directory "snippets" "rust-snippets" "snippets"))
(defvar clojure-snippets (joindirs user-emacs-directory "snippets" "clojure-snippets" "snippets"))
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '(yasnippet-snippets my-snippets go-snippets))
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


;; web-mode.el is an autonomous emacs major-mode for editing web templates
(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook '(lambda ()
			    (setq web-mode-enable-auto-pairing t)
			    (setq web-mode-enable-auto-closing t)
			    (setq web-mode-enable-css-colorization t)
			    (setq web-mode-ac-sources-alist
				  '(("css" . (ac-source-css-property))
				    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
			    (local-set-key (kbd "RET") 'newline-and-indent)
			    (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match))))


;; Emacs and scheme talk to each other
(use-package ac-geiser
  :defer t
  :config
  (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup))

(use-package geiser
  :defer t
  :config
  (setq geiser-active-implementations '(racket))
  (use-package ac-geiser)
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'geiser-repl-mode))
  (use-package paredit)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))


;; A face dedicated to lisp parentheses
(use-package paren-face
  :config (global-paren-face-mode))


;; Emacs powerline and base16 theme
(use-package base16-theme
  :when (display-graphic-p)
  :config
  (use-package powerline)
  (load-theme 'base16-eighties t))

(use-package powerline
  :defer t
  :config (powerline-default-theme))


;; Emacs OCaml mode
(use-package tuareg
  :defer t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'tuareg-mode-hook #'yas-minor-mode)
  (use-package merlin)
  (use-package utop))

(use-package merlin
  :defer t
  :config
  (setq merlin-ac-setup t)
  (add-hook 'merlin-mode-hook (lambda ()
				(local-set-key (kbd "M-.") 'merlin-locate)
				(local-set-key (kbd "M-,") 'merlin-pop-stack)))
  (add-hook 'caml-mode-hook 'merlin-mode))

(use-package utop
  :defer t
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))


;; Emacs configuration for Rust
(use-package rust-mode
  :defer t
  :config
  (use-package cargo)
  (use-package flycheck-rust)
  (add-hook 'rust-mode-hook '(lambda ()
			       (yas-minor-mode 1)
			       (rust-enable-format-on-save)
			       (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(use-package flycheck-rust :defer t)


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
  (add-hook 'clojure-mode-hook '(lambda ()
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
				  (company-mode)))
  (add-hook 'inf-clojure-mode-hook #'eldoc-mode))


;; Emacs as a C/C++ Editor/IDE
(use-package auto-complete-c-headers
  :defer t
  :config
  (defun my:ac-c-headers-init ()
    "."
    (require 'auto-complete-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers))
  (my:ac-c-headers-init))

(use-package xcscope
  :defer t
  :config
  (cscope-setup))

(add-hook 'c-mode-hook '(lambda ()
			  (use-package auto-complete-c-headers)
			  (yas-minor-mode 1)
			  (use-package iedit)
			  (local-set-key (kbd "C-c ;") 'iedit-mode)
			  (use-package xcscope)))


;; Emacs major mode for editing Lua
(use-package lua-mode
  :config
  (add-hook 'lua-mode-hook '(lambda ()
			      (use-package company)
			      (use-package company-lua)
			      (setq-local company-backends '(company-lua))
			      (use-package yasnippet)
			      (yas-minor-mode))))

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
  (use-package yasnippet)
  (yas-minor-mode)
  (use-package py-autopep8))

(use-package py-autopep8
  :defer t
  :config
  (py-autopep8-enable-on-save)
  (setq py-autopep8-options '("--max-line-length=120")))


;; Disable the mouse in Emacs
(use-package disable-mouse
  :diminish (disable-mouse-mode global-disable-mouse-mode)
  :config
  (global-disable-mouse-mode))


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


(provide 'init-melpa)
;;; init-melpa.el ends here
