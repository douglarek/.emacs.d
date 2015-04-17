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
		      cider
		      clj-refactor
		      coffee-mode
		      company
		      discover-my-major
		      dockerfile-mode
		      drag-stuff
		      edts
		      ein
		      ensime
		      flx-ido
		      flycheck
		      flycheck-pos-tip
		      flycheck-rust
		      geiser
		      git-gutter
		      git-messenger
		      haskell-mode
		      flycheck-haskell
		      htmlize
		      ido-vertical-mode
		      jedi
		      jist
		      markdown-mode
		      magit
		      move-dup
		      multiple-cursors
		      org
		      paradox
		      popup
		      projectile
		      py-autopep8
		      py-isort
		      rust-mode
		      scala-mode2
		      scala-outline-popup
		      slime
		      smartparens
		      smex
		      sourcemap
		      thingatpt+
		      toml-mode
		      transpose-mark
		      tss
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
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
	("blade"  . "\\.blade\\."))
)

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
      flycheck-flake8-maximum-line-length 100)

;; Improved Haskell support for Flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Flycheck errors display in tooltip
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


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
  (setq inferior-lisp-program "sbcl"))
(setq slime-contribs '(slime-fancy))

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

(setq yas-snippet-dirs '(clojure-snippets rust-snippets yasnippet-snippets))

(yas-global-mode 1)

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
(add-hook 'before-save-hook 'py-autopep8-before-save)
(setq py-autopep8-options '("--max-line-length=100"))

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


;; virtualenv tool for emacs
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))


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
(add-hook 'after-init-hook 'my-after-init-hook)
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


(provide 'init-melpa)
;;; init-melpa.el ends here
