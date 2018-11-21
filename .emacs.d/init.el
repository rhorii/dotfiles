;;; init.el --- init.el
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(blink-cursor-mode -1)
(column-number-mode +1)
(global-hl-line-mode +1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq scroll-conservatively 1)
(setq-default indent-tabs-mode nil)
(set-fontset-font nil 'unicode (font-spec :family "Ricty Diminished" :size 14))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(use-package company
  :ensure t
  :config
  (global-company-mode +1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode +1))

(use-package counsel
  :ensure t
  :demand t
  :bind (([remap isearch-forward] . swiper)
         ([remap isearch-backward] . swiper))
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode +1)

  (setq counsel-describe-function-function 'helpful-callable)
  (setq counsel-describe-variable-function 'helpful-variable)
  (counsel-mode +1))

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package exec-path-from-shell
  :ensure t
  :if window-system
  :config
  (exec-path-from-shell-initialize))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode +1))

(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger-mode))

(use-package helpful
  :ensure t
  :bind ([remap describe-key] . helpful-key))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode +1))

(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

(use-package json-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (setq ledger-post-amount-alignment-column 62))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package minions
  :ensure t
  :config
  (setq minions-direct '(flycheck-mode))
  (minions-mode +1))

(use-package ns-auto-titlebar
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (ns-auto-titlebar-mode +1))

(use-package ob
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (setq paradox-execute-asynchronously t))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 512)
  (recentf-mode +1))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package solarized-theme
  :ensure t
  :if window-system
  :config
  (setq solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package windmove
  :disabled t
  :config
  (windmove-default-keybindings))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :ensure t)

(use-package yari
  :ensure t
  :bind (:map help-map ("R" . yari)))

(use-package zenburn-theme
  :disabled t
  :ensure t
  :if window-system
  :config
  (load-theme 'zenburn t))

;;; init.el ends here
