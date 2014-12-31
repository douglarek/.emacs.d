;;; init-melpa.el --- Melpa init file.

;;; Commentary:

;; This file is used to manage melpa packages' settings.

;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Refresh the packages descriptions
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to load
(setq package-load-list '(all))

;; Install all needed
(package-install 'jedi)
(package-install 'ein)
(package-install 'smex)
(package-install 'whitespace-cleanup-mode)
(package-install 'git-gutter)
(package-install 'web-mode)
(package-install 'haskell-mode)
(package-install 'magit)
(package-install 'gist)
(package-install 'flycheck)
(package-install 'smartparens)
(package-install 'yasnippet)
(package-install 'company)
(package-install 'window-number)
(package-install 'ido-vertical-mode)

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

;; whitespace-cleanup -> In Emacs, intelligently call whitespace-cleanup on save

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
(setq flycheck-check-syntax-automatically '(new-line))

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

(provide 'init-melpa)
;;; init-melpa.el ends here
