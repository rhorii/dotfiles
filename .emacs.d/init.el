;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

;; Better Defaults
(custom-set-variables
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(create-lockfiles nil)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(require-final-newline t)
 '(scroll-conservatively 10000)
 '(scroll-bar-mode nil)
 '(tab-width 2)
 '(tool-bar-mode nil))

;; coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; font
(set-face-attribute 'default nil :family "Ricty Diminished" :height 140)
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))

;; `package'
(require 'package)
(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; packages
(use-package async
  :ensure t
  :defer t)

(use-package autorevert
  :config
  (global-auto-revert-mode +1))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :ensure t
  :config
  (company-quickhelp-mode +1))

(use-package counsel
  :after ivy
  :ensure t
  :config
  (counsel-mode +1))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))

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

(use-package elec-pair
  :config
  (electric-pair-mode +1))

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
  :ensure t
  :config
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pycompile-executable "python3")
  (global-flycheck-mode +1))

(use-package flycheck-ledger
  :after (flycheck ledger-mode)
  :ensure t)

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

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
  :config
  (global-hl-line-mode +1))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (ivy-mode +1))

(use-package json-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (setq ledger-post-amount-alignment-column 62)
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
  :config
  (setq minions-direct '(flycheck-mode))
  (minions-mode +1))

(use-package org
  :bind
  ("C-c l" . org-store-link)
  :init
  (setq org-replace-disputed-keys t)
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-agenda-files '("~/org"))
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-tag-alist
        '(("@office" . ?o)
          ("@home" . ?h)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package org-agenda
  :bind
  ("C-c a" . org-agenda))

(use-package org-capture
  :bind
  ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Task" entry
           (file "~/org/inbox.org")
           "* TODO %?\n%U")
          ("n" "Note" entry
           (file "~/org/inbox.org")
           "* %?\n%U"))))

(use-package org-habit
  :defer t
  :config
  (setq org-habit-graph-column 80))

(use-package org-src
  :defer t
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-src-window-setup 'current-window))

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 512)
  (recentf-mode +1))

(use-package ruby-mode
  :defer t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package savehist
  :config
  (savehist-mode +1))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package shr
  :defer t
  :config
  (setq shr-use-fonts nil))

(use-package smex
  :ensure t
  :defer t)

(use-package solarized-theme
  :if window-system
  :ensure t
  :config
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
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

(use-package winner
  :config
  (winner-mode +1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

;;; init.el ends here
