;;; init-melpa.el --- Melpa init file.

;;; Commentary:

;; This file is used to manage melpa packages' settings.

;;; Code:

(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.zilongshanren.com/gnu/")
			 ("melpa" . "https://elpa.zilongshanren.com/melpa/")))
(package-initialize)

;; Refresh the packages descriptions
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to load
(setq package-load-list '(all))

;; Install all needed
(defvar my-packages '(ac-emoji
		      ac-geiser
		      auto-complete-c-headers
		      aggressive-indent
		      avy
		      base16-theme
		      bing-dict
		      cider
		      clj-refactor
		      clojure-mode
		      company
		      counsel
		      exec-path-from-shell
		      flycheck
		      flycheck-pos-tip
		      geiser
		      git-gutter
		      git-messenger
		      go-autocomplete
		      go-eldoc
		      go-impl
		      golint
		      go-guru
		      go-mode
		      go-rename
		      iedit
		      imenu-anywhere
		      ix
		      inf-clojure
		      jedi
		      lua-mode
		      company-lua
		      kotlin-mode
		      magit
		      merlin
		      move-dup
		      multiple-cursors
		      paredit
		      paren-face
		      plur
		      powerline
		      projectile
		      py-autopep8
		      cargo
		      flycheck-rust
		      rainbow-delimiters
		      rust-mode
		      smartparens
		      smex
		      swap-regions
		      swiper
		      tuareg
		      undo-tree
		      utop
		      web-mode
		      whole-line-or-region
		      w3m
		      xcscope
		      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))


;; Jump to things in Emacs tree-style
(define-key global-map (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)


;; Make Emacs use the $PATH set up by the user's shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (let ((envs '("GOROOT" "GOPATH" "PYTHONPATH")))
    (exec-path-from-shell-copy-envs envs)))


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))

;; Flycheck errors display in tooltip, only when display-graphic-p
(when (display-graphic-p)
  (eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))


;; git-gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(global-git-gutter-mode t)


;; ido/helm imenu tag selection across all buffers with the same mode
(global-set-key (kbd "C-c C-j") 'imenu-anywhere)


;; Emacs Port of git-messenger.vim
(require 'git-messenger)
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)


;; An improved Go mode for emacs
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "M-.") 'godef-jump) ; "M-*" back
			   (make-local-variable 'before-save-hook)
			   (setq gofmt-command "goimports")
			   (add-hook 'before-save-hook 'gofmt-before-save)
			   (define-key go-mode-map (kbd "C-c C-j") nil)
			   (yas-minor-mode 1)))

;; Go completion for Emacs
(require 'go-autocomplete)

;; Eldoc for Go
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)


;; Project Interaction Library for Emacs
(projectile-global-mode)


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
(defvar go-snippets (joindirs user-emacs-directory "snippets" "go-snippets" "snippets"))
(defvar rust-snippets (joindirs user-emacs-directory "snippets" "rust-snippets" "snippets"))
(defvar clojure-snippets (joindirs user-emacs-directory "snippets" "clojure-snippets" "snippets"))
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))
(setq yas-snippet-dirs '(yasnippet-snippets my-snippets go-snippets))

(require 'yasnippet)
(yas-reload-all)


;; An Intelligent auto-completion extension for Emacs
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map "\r" 'ac-complete)
(define-key ac-completing-map "\t" nil)


;; Emacs isearch with an overview. Oh, man!
(ivy-mode 1)
(global-set-key "\C-s" 'swiper)
(advice-add 'swiper :after '(lambda () (recenter)))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)


;; web-mode.el is an autonomous emacs major-mode for editing web templates
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook '(lambda ()
			    (setq web-mode-enable-auto-pairing t)
			    (setq web-mode-enable-auto-closing t)
			    (setq web-mode-enable-css-colorization t)
			    (setq web-mode-ac-sources-alist
				  '(("css" . (ac-source-css-property))
				    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
			    (local-set-key (kbd "RET") 'newline-and-indent)
			    (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)))


;; Emacs and scheme talk to each other
(setq geiser-active-implementations '(racket))
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'geiser-repl-mode))
(add-hook 'scheme-mode-hook #'enable-paredit-mode)


;; A face dedicated to lisp parentheses
(global-paren-face-mode)


;; Emacs powerline and base16 theme
(when (display-graphic-p)
  (powerline-default-theme)
  (load-theme 'base16-eighties-dark t))


;; Emacs OCaml mode
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook #'yas-minor-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
(setq merlin-ac-setup t)
(add-hook 'merlin-mode-hook (lambda ()
			      (local-set-key (kbd "M-.") 'merlin-locate)
			      (local-set-key (kbd "M-,") 'merlin-pop-stack)))
(setq utop-command "opam config exec -- utop -emacs")
(add-hook 'tuareg-mode-hook 'utop-minor-mode)


;; Emacs configuration for Rust
(add-hook 'rust-mode-hook '(lambda ()
			     (yas-minor-mode 1)
			     (rust-enable-format-on-save)
			     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


;; Emacs support for the Clojure(Script) programming language
(add-hook 'clojure-mode-hook '(lambda ()
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
				(cljr-add-keybindings-with-prefix "C-c C-m")))
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)


;; Emacs as a C/C++ Editor/IDE
(defun my:ac-c-headers-init ()
  "."
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c-mode-hook '(lambda ()
			  (my:ac-c-headers-init)
			  (yas-minor-mode 1)
			  (local-set-key (kbd "C-c ;") 'iedit-mode)
			  (cscope-setup)))


;; Emacs major mode for editing Lua
(add-hook 'lua-mode-hook '(lambda ()
			   (company-mode)
			   (setq-local company-backends '(company-lua))
			   (yas-minor-mode)))


;; Python auto-completion for Emacs
(add-hook 'python-mode-hook '(lambda ()
			       (jedi:setup)
			       (setq jedi:complete-on-dot t)
			       (local-set-key (kbd "M-.") 'jedi:goto-definition)
			       (local-set-key (kbd "M-,") 'goto-definition-pop-marker)
			       (local-set-key (kbd "C-c d") 'jedi:show-doc)
			       (yas-minor-mode)
			       (py-autopep8-enable-on-save)
			       (setq py-autopep8-options '("--max-line-length=120"))))


(provide 'init-melpa)
;;; init-melpa.el ends here
