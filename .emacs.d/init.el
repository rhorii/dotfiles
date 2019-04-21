;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

;; proxy settings
(load (expand-file-name "proxy.el" user-emacs-directory) t)

;; `package'
(require 'package)
(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; packages
(use-package async
  :ensure t
  :defer t)

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (global-company-mode t))

(use-package company-quickhelp
  :after company
  :ensure t
  :custom
  (company-quickhelp-mode t))

(use-package counsel
  :after ivy
  :ensure t
  :custom
  (counsel-mode t))

(use-package csv-mode
  :ensure t
  :defer t)

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package docker-tramp
  :ensure t
  :defer t
  :custom
  (docker-tramp-use-names t))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (indent-tabs-mode nil)
  (load-prefer-newer t)
  (scroll-conservatively 10000)
  (tab-width 2)
  (tool-bar-mode nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "Ricty" :height 140)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty")))

(use-package files
  :custom
  (auto-save-default nil)
  (confirm-kill-emacs 'yes-or-no-p)
  (make-backup-files nil)
  (require-final-newline t))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-python-flake8-executable "flake8")
  (flycheck-python-pycompile-executable "python3")
  (global-flycheck-mode t))

(use-package flycheck-ledger
  :after (flycheck ledger-mode)
  :ensure t)

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode))

(use-package hl-line
  :custom
  (global-hl-line-mode t))

(use-package hydra
  :ensure t
  :defer t)

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ivy
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-mode t))

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
  :custom
  (ledger-post-amount-alignment-column 62)
  (ledger-reports
   '(("Balance Sheet" "%(binary) -f %(ledger-file) bal --explicit --pedantic --cleared ^資産 ^負債 ^資本")
     ("Monthly Balance" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly ^資産 ^負債 ^資本 --collapse")
     ("Monthly Expence" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly ^支出 --sort -amount")
     ("Account Statement" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared ^%(account)")))
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
  :ensure t
  :custom
  (minions-direct '(flycheck-mode))
  (minions-mode t))

(use-package mule
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8))

(use-package org
  :ensure org-plus-contrib
  :bind
  ("C-c l" . org-store-link)
  :custom
  (org-agenda-files '("~/org"))
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-babel-load-languages '((emacs-lisp . t) (shell . t)))
  (org-default-notes-file "~/org/inbox.org")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-modules '(org-info org-depend))
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-replace-disputed-keys t)
  (org-tag-alist '(("@office" . ?o) ("@home" . ?h)))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)"))))

(use-package org-agenda
  :bind
  ("C-c a" . org-agenda))

(use-package org-capture
  :bind
  ("C-c c" . org-capture)
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file "~/org/inbox.org") "* TODO %?\n%U")
     ("n" "Note" entry (file "~/org/inbox.org") "* %?\n%U"))))

(use-package org-src
  :defer t
  :custom
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window))

(use-package paradox
  :ensure t
  :defer t
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t))

(use-package paren
  :custom
  (show-paren-mode t))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode t))

(use-package recentf
  :custom
  (recentf-max-saved-items 512)
  (recentf-mode t))

(use-package ruby-mode
  :defer t
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package savehist
  :custom
  (savehist-mode t))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package sh-script
  :defer t
  :custom
  (sh-basic-offset 2))

(use-package shr
  :defer t
  :custom
  (shr-use-fonts nil))

(use-package simple
  :custom
  (column-number-mode t))

(use-package smex
  :ensure t
  :defer t)

(use-package solarized-theme
  :if window-system
  :ensure t
  :custom
  (solarized-scale-org-headlines nil)
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (load-theme 'solarized-dark t))

(use-package swiper
  :after ivy
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package undo-tree
  :ensure t
  :custom
  (global-undo-tree-mode t))

(use-package which-key
  :ensure t
  :custom
  (which-key-mode t))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package winner
  :custom
  (winner-mode t))

(use-package yasnippet
  :ensure t
  :custom
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

;;; init.el ends here
