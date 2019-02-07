;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration.

;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Better Defaults
(blink-cursor-mode -1)
(column-number-mode +1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq auto-save-default nil)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq make-backup-files nil)
(setq require-final-newline t)
(setq scroll-conservatively 10000)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Font
(set-face-attribute 'default nil :family "Ricty Diminished" :height 140)
(set-fontset-font t 'unicode (font-spec :family "Ricty Dminished"))

;; Customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (shell . t))))

(use-package async
  :ensure t
  :defer t)

(use-package autorevert
  :config
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode +1))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (global-company-mode +1))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode +1))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (setq counsel-describe-function-function 'helpful-callable)
  (setq counsel-describe-variable-function 'helpful-variable)
  (counsel-mode +1))

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package eww
  :defer t
  :config
  (setq shr-use-fonts nil))

(use-package exec-path-from-shell
  :if
  (memq window-system '(mac ns x))
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
  :ensure t
  :config
  (setq flycheck-python-flake8-executable "flake8")
  (global-flycheck-mode +1))

(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger-mode))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package helpful
  :ensure t
  :bind
  ("C-h k" . helpful-key))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode))

(use-package highlight-symbol
  :ensure t
  :hook
  (prog-mode . highlight-symbol-mode)
  (prog-mode . highlight-symbol-nav-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ivy
  :ensure t
  :demand t
  :commands ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (ivy-mode +1))

(use-package ivy-rich
  :ensure t
  :after ivy
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
  :bind
  ("C-c m" . magit-status))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package minions
  :ensure t
  :config
  (setq minions-direct '(flycheck-mode))
  (minions-mode +1))

(use-package osx-dictionary
  :ensure t
  :defer t)

(use-package paradox
  :ensure t
  :demand t
  :commands paradox-enable
  :config
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (paradox-enable))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 512)
  (recentf-mode +1))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package solarized-theme
  :if window-system
  :ensure t
  :config
  (setq solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

(use-package swiper
  :ensure t
  :bind
  ([remap isearch-forward] . swiper)
  ([remap isearch-backward] . swiper))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode +1))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; init.el ends here
