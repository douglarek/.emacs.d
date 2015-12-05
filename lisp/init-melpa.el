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
(defvar my-packages '(ack
		      ace-jump-mode
		      ace-jump-buffer
		      bing-dict
		      4clojure
		      cider
		      clj-refactor
		      coffee-mode
		      company
		      dash-at-point
		      discover-my-major
		      dockerfile-mode
		      drag-stuff
		      edts
		      ein
		      ensime
		      exec-path-from-shell
		      flx-ido
		      flycheck
		      flycheck-pos-tip
		      flycheck-rust
		      geiser
		      git-gutter
		      git-messenger
		      git-timemachine
		      go-complete
		      go-eldoc
		      go-mode
		      groovy-mode
		      haskell-mode
		      flycheck-haskell
		      htmlize
		      ido-vertical-mode
		      imenu-anywhere
		      javadoc-lookup
		      jdee
		      jedi
		      jist
		      lua-mode
		      markdown-mode
		      magit
		      monokai-theme
		      move-dup
		      multiple-cursors
		      paradox
		      popup
		      projectile
		      py-autopep8
		      py-isort
		      rainbow-delimiters
		      restclient
		      rust-mode
		      scala-mode2
		      scala-outline-popup
		      slime
		      smartparens
		      smex
		      sourcemap
		      sx
		      thingatpt+
		      toml-mode
		      transpose-mark
		      tss
		      tuareg
		      undo-tree
		      visual-regexp
		      virtualenvwrapper
		      web-mode
		      whitespace-cleanup-mode
		      window-number
		      yaml-mode
		      yasnippet
		      whole-line-or-region)
  )

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; jedi -> Python auto-completion for Emacs.
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq jedi:complete-on-dot t)

;; smex -> A smart M-x enhancement for Emacs.
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; git-gutter -> Emacs port of GitGutter which is Sublime Text Plugin
(global-git-gutter-mode t)

;; web-mode -> web template editing mode for Emacs
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Mode hook for web development."
  (require 'web-mode)
  ; html indention
  (setq web-mode-markup-indent-offset 2)
  ; css indention
  (setq web-mode-css-indent-offset 2)
  ; script indention
  (setq web-mode-code-indent-offset 2)
  ; tag attributes indention
  (setq web-mode-attr-indent-offset 2)
  ; CSS colorization
  (setq web-mode-enable-css-colorization t)
  ; Autopairing
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq-default indent-tabs-mode nil))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Emacs mode for Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(setq haskell-stylish-on-save t)

;; Flycheck support for pyflakes
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))
;; Better Rust/Cargo support for Flycheck
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Python only
(setq flycheck-python-flake8-executable (executable-find "flake8")
      flycheck-flake8-maximum-line-length 120)

;; Improved Haskell support for Flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Flycheck errors display in tooltip, only when display-graphic-p
(when (display-graphic-p)
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; Select windows by numbers -> window-number
(require 'window-number)
(window-number-mode t)
;; `window-number-switch` same as `other-window` (C-x o) when windows less than three;
;; so binding `window-number-switch` on 'C-x o' to instead `other-window`.
(global-set-key (kbd "C-x o") 'window-number-switch)

;; Makes ido-mode display vertically -> ido-vertical-mode.el
(if ido-mode
    (ido-vertical-mode t))

;; The emacs major mode for editing files in the YAML data serialization format.
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; The Superior Lisp Interaction Mode for Emacs
(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl --noinform --no-linedit"))
(setq slime-contribs '(slime-fancy))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;; ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
      'paredit-close-round)


;; Emacs major mode for editing Markdown files
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens
			nil t))))

(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "markdown | smartypants"))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(smartparens-global-mode t)

;; Emacs and scheme talk to each other
(setq geiser-impl-installed-implementations '(guile))

;; A template system for Emacs
(defun joindirs (root &rest dirs)
  (if (not dirs)
      root
    (apply 'joindirs
	   (expand-file-name (car dirs) root)
	   (cdr dirs))))

(defvar clojure-snippets (joindirs user-emacs-directory "snippets" "clojure-snippets"))
(defvar rust-snippets (joindirs user-emacs-directory "snippets" "rust-snippets" "snippets"))
(defvar yasnippet-snippets (joindirs user-emacs-directory "snippets" "yasnippet-snippets"))
(defvar my-snippets (joindirs user-emacs-directory "snippets" "my-snippets"))

(setq yas-snippet-dirs '(clojure-snippets rust-snippets yasnippet-snippets my-snippets))

(require 'yasnippet)
(yas-reload-all)
(dolist (hook '(java-mode-hook clojure-mode-hook))
  (add-hook hook #'yas-minor-mode)
  )


;; Emacs Interface to Ack-like Tools
(defvar ack-history nil
  "History for the `ack` command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack --nogroup --with-filename "))
     (list (read-shell-command "Run ack (like this): "
			       ack-command
			       'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
		       'grep-mode)))

;; Using ThingAtPoint and the Existing C-s C-w
;; http://www.emacswiki.org/emacs/SearchAtPoint
(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
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

;; Operate on current line if region undefined
(whole-line-or-region-mode t)

;; Tidy up the current buffer according to Python’s PEP8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=120"))

;; Treat undo history as a tree
(global-undo-tree-mode t)


;; CIDER is a Clojure IDE and REPL for Emacs
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)

;; Use company-mode to enable auto-completion inside of source code and REPL buffers
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Adjust CIDER experience
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; => ")
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-display-in-current-window t)
(setq cider-prefer-local-resources t)
(setq cider-stacktrace-fill-column 80)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-show-error-buffer nil)

;; A quick cursor jump mode for emacs
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; Project for modernizing Emacs' Package Menu
(eval-after-load 'paradox
  '(custom-set-variables
    '(paradox-github-token t)))


;; Org: an Emacs Mode for Notes, Planning, and Authoring
(setq org-tag-alist '(("clojure" . ?c)
		      ("git" . ?g)
		      ("@home" . ?h)
		      ("python" . ?p)
		      ("rust" . ?r)
		      ("@work" . ?w)))
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "DELEGATED")))
(setq org-todo-keyword-faces
      '(("DOING" . "yellow")))

(setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ditaa . t)
   ))

(setq org-completion-use-ido t
      org-confirm-babel-evaluate nil
      org-html-validation-link nil
      org-log-done 'time
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-startup-indented t)


;; Drag stuff around in Emacs
(drag-stuff-mode t)


;; Emacs Port of git-messenger.vim
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
(setq magit-last-seen-setup-instructions "1.4.0")


;; A collection of simple Clojure refactoring functions for Emacs
(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
(add-hook 'cider-connected-hook #'cljr-update-artifact-cache)
(add-hook 'cider-connected-hook #'cljr-warm-ast-cache)
(setq cljr-sort-comparator 'cljr--semantic-comparator)
(cljr-add-keybindings-with-prefix "C-c C-m")


;; Project Interaction Library for Emacs
(projectile-global-mode)

;; Fuzzy matching for Emacs
(flx-ido-mode t)

;; disable ido faces to see flx highlights
(setq ido-enable-flex-matching t)
(setq flx-ido-use-faces nil)


;; Emacs Major Mode for CoffeeScript
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

;; If you want to remove sourcemap file after jumping corresponding point
(defun my/coffee-after-compile-hook (props)
  (sourcemap-goto-corresponding-point props)
  (delete-file (plist-get props :sourcemap)))
(add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)

(eval-after-load 'coffee-mode
  '(custom-set-variables
    '(coffee-tab-width 2)
    '(coffee-args-compile '("-c" "--bare"))))


;; Emacs WhiteSpace mode
(global-whitespace-mode t)

;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))

;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))


;; A new scala-mode for emacs24
(add-hook 'scala-mode-hook '(lambda ()
			      (local-set-key (kbd "RET") 'newline-and-indent)
			      (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)
			      ))
(add-hook 'scala-mode-hook '(lambda ()
			      ; (require 'whitespace)
			      (make-local-variable 'before-save-hook)
			      (add-hook 'before-save-hook 'whitespace-cleanup)
			      (whitespace-mode)
			      ))

;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)


;; Scala file summary popup
(require 'popup)
(define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
(require 'scala-outline-popup)
(setq scala-outline-popup-select 'closest)


;; ENhanced Scala Interaction Mode for Emacs
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; Multiple cursors for emacs
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)


;; Erlang Development Tool Suite
(add-hook 'erlang-mode-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start))


;; Provide a interface for auto-complete.el/flymake.el on typescript-mode.
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Do setting recommemded configuration
(tss-config-default)


;; Super fast Emacs buffer switching extension for ace-jump-mode
(global-set-key (kbd "M-g b") 'ace-jump-buffer)


;; Emacs GUI settings for osx
(when (memq window-system '(mac ns))
  (load-theme 'monokai t)
  (exec-path-from-shell-initialize))


;; Imenu mode for Rust, Python, ...
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "C-c C-j") 'imenu-anywhere)))
(global-set-key (kbd "C-c C-j") 'imenu-anywhere)


;; Emacs OCaml mode
;; Indent `=' like a standard keyword.
(setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
(setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
(setq tuareg-in-indent 0)

(add-hook 'tuareg-mode-hook
	  ;; Turn on auto-fill minor mode.
	  (lambda () (auto-fill-mode 1)))


;; A emacs lisp for searching the word at point with Dash
(add-hook 'java-mode-hook
	  (lambda () (setq dash-at-point-docset "java")))


;; Open and evaluate 4clojure.com questions in emacs
; from http://sachachua.com/blog/2014/05/playing-around-clojure-cider-4clojure/
(defun my/4clojure-check-and-proceed ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (let ((result (4clojure-check-answers)))
    (unless (string-match "failed." result)
      (4clojure-next-question))))
(define-key clojure-mode-map (kbd "C-c C-c") 'my/4clojure-check-and-proceed)


;; An improved Go mode for emacs
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			   (local-set-key (kbd "C-c C-i") 'go-goto-imports)
			   (local-set-key (kbd "M-.") 'godef-jump) ; "M-*" back
			   ; (make-local-variable 'before-save-hook)
			   ; (add-hook 'before-save-hook 'gofmt-before-save)
			   (local-set-key (kbd "C-c C-f") 'gofmt)))


;; Native Go completion for Emacs
(require 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)


;; Eldoc for Go
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)


(provide 'init-melpa)
;;; init-melpa.el ends here
