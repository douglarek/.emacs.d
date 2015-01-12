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
(package-install 'ack)
(package-install 'company)
(package-install 'ein)
(package-install 'flycheck)
(package-install 'flycheck-rust)
(package-install 'geiser)
(package-install 'gist)
(package-install 'git-gutter)
(package-install 'haskell-mode)
(package-install 'ido-vertical-mode)
(package-install 'jedi)
(package-install 'markdown-mode)
(package-install 'magit)
(package-install 'move-dup)
(package-install 'org)
(package-install 'rust-mode)
(package-install 'slime)
(package-install 'smartparens)
(package-install 'smex)
(package-install 'thingatpt+)
(package-install 'toml-mode)
(package-install 'web-mode)
(package-install 'whitespace-cleanup-mode)
(package-install 'window-number)
(package-install 'yaml-mode)
(package-install 'yasnippet)
(package-install 'whole-line-or-region)

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

;; Flycheck support for pyflakes
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))
;; Better Rust/Cargo support for Flycheck
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Python only
(setq flycheck-python-flake8-executable (executable-find "flake8")
      flycheck-flake8-maximum-line-length 120)

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
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens
			nil t))))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(smartparens-global-mode t)

;; Emacs and scheme talk to each other
(setq geiser-impl-installed-implementations '(guile))

;; A template system for Emacs
(defvar snippets-directory (concat user-emacs-directory "snippets"))
(setq yas-snippet-dirs '(snippets-directory))
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

(provide 'init-melpa)
;;; init-melpa.el ends here
