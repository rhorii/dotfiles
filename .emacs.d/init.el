;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

(progn ;     custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))

(progn ;     local.el
  (let ((local-file (expand-file-name "local.el" user-emacs-directory)))
    (load local-file t)))

(progn ;     coding
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8))

(progn ;     font
  (cond
   ((eq window-system 'ns)
    (set-face-attribute 'default nil :family "Ricty" :height 140))
   ((eq window-system 'x)
    (set-face-attribute 'default nil :family "Ricty" :height 120)))
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty")))

(progn ;    `package'
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(progn ;    `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(use-package async
  :ensure t
  :defer t)

(use-package company
  :ensure t)

(use-package company-quickhelp
  :after company
  :ensure t)

(use-package counsel
  :after ivy
  :ensure t)

(use-package csv-mode
  :ensure t
  :defer t)

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package docker-tramp
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t)

(use-package flycheck-ledger
  :after (flycheck ledger-mode)
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable))

(use-package hydra
  :ensure t
  :defer t)

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ivy
  :ensure t)

(use-package ivy-hydra
  :after (hydra ivy)
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (add-hook 'ledger-mode-hook
            (lambda() (add-hook 'before-save-hook
                                'ledger-mode-clean-buffer nil t))))

(use-package magit
  :ensure t
  :bind
  ("C-c m" . magit-status))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package minions
  :ensure t)

(use-package mozc
  :if (memq window-system '(x)))

(use-package org
  :ensure org-plus-contrib
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(use-package paradox
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package shackle
  :ensure t
  :config
  (shackle-mode +1))

(use-package smex
  :ensure t
  :defer t)

(use-package solarized-theme
  :if window-system
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package swiper
  :after ivy
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

;;; init.el ends here
