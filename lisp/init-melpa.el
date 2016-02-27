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
(defvar my-packages '(ac-emoji
		      ac-geiser
		      avy
		      base16-theme
		      bing-dict
		      cargo
		      circe
		      company
		      company-racer
		      counsel
		      exec-path-from-shell
		      flycheck
		      flycheck-pos-tip
		      flycheck-rust
		      geiser
		      git-gutter
		      git-messenger
		      go-autocomplete
		      go-eldoc
		      golint
		      go-mode
		      gorepl-mode
		      imenu-anywhere
		      ix
		      magit
		      move-dup
		      multiple-cursors
		      paredit
		      paren-face
		      powerline
		      projectile
		      racer
		      restclient
		      rustfmt
		      rust-mode
		      ensime
		      sbt-mode
		      scala-mode2
		      smartparens
		      smex
		      swiper
		      undo-tree
		      web-mode
		      whole-line-or-region
		      w3m
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
  (let ((envs '("GOROOT" "GOPATH")))
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
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			   (local-set-key (kbd "C-c C-i") 'go-goto-imports)
			   (local-set-key (kbd "M-.") 'godef-jump) ; "M-*" back
			   (make-local-variable 'before-save-hook)
			   (setq gofmt-command "goimports")
			   (add-hook 'before-save-hook 'gofmt-before-save)
			   (local-set-key (kbd "C-c C-f") 'gofmt)
			   (when (executable-find "oracle")
			     (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
			   (when (executable-find "gorename")
			     (load-file "$GOPATH/src/golang.org/x/tools/refactor/rename/go-rename.el"))))

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
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))
(setq yas-snippet-dirs '(yasnippet-snippets my-snippets go-snippets rust-snippets))

(require 'yasnippet)
(yas-reload-all)
(dolist (hook '(go-mode-hook rust-mode-hook))
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


;; A major Emacs mode for editing Rust source code
;; *NOTE* I use RUST_SRC_PATH (Environment variable) for racer rather than set it in Emacs.
;; Please see http://git.io/vukHq for more detail.
(add-hook 'rust-mode-hook '(lambda ()
			     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
			     (racer-mode)
			     (racer-turn-on-eldoc)
			     (company-mode)
			     (setq company-tooltip-align-annotations t)
			     (set (make-local-variable 'company-backends) '(company-racer))
			     (local-set-key (kbd "M-.") #'racer-find-definition)
			     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
			     (rustfmt-enable-on-save)
			     (cargo-minor-mode)))


;; An Intelligent auto-completion extension for Emacs
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map "\r" 'ac-complete)
(define-key ac-completing-map "\t" nil)


;; Emacs isearch with an overview. Oh, man!
(ivy-mode 1)
(global-set-key "\C-s" 'swiper)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")


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


;;ENhanced Scala Interaction Mode for Emacs
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook #'yas-minor-mode)
(add-hook 'scala-mode-hook '(lambda ()
			      (local-set-key (kbd "M-.") 'sbt-find-definitions)
			      (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
			      (auto-complete-mode 0)))
(add-hook 'sbt-mode-hook '(lambda ()
			    (setq compilation-skip-threshold 1)
			    (local-set-key (kbd "C-a") 'comint-bol)
			    (local-set-key (kbd "M-RET") 'comint-accumulate)))


(provide 'init-melpa)
;;; init-melpa.el ends here
