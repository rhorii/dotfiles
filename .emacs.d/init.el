;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

;; Better Defaults
(blink-cursor-mode -1)
(column-number-mode +1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq auto-save-default nil)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq require-final-newline t)
(setq scroll-conservatively 10000)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; font
(set-face-attribute 'default nil :family "Ricty Diminished" :height 140)
(set-fontset-font t 'unicode (font-spec :family "Ricty Dminished"))

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
  :after company
  :ensure t
  :config
  (company-quickhelp-mode +1))

(use-package counsel
  :after ivy
  :ensure t
  :config
  (counsel-mode +1))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

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
  :bind ("C-=" . er/expand-region))

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
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

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
  :bind ("C-c m" . magit-status))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package minions
  :ensure t
  :config
  (setq minions-direct '(flycheck-mode))
  (minions-mode +1))

(use-package org
  :bind (("C-c l" . org-store-link))
  :config
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-tag-alist
        '((:startgroup)
          ("@home" . ?h)
          ("@office" . ?o)
          ("@errands" . ?e)
          (:endgroup)
          ("note" . ?n)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")))
  (setq org-todo-state-tags-triggers
        '(("TODO" ("WAITING") ("CANCELED"))
          ("NEXT" ("WAITING") ("CANCELED"))
          ("DONE" ("WAITING") ("CANCELED"))
          ("WAITING" ("WAITING" . t) ("CANCELED"))
          ("CANCELED" ("WAITING") ("CANCELED" . t))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-custom-commands
        '(("n" "Notes" tags "+note")
          (" " "Agenda"
           ((agenda "")
            (tags "+INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-WAITING-SOMEDAY-CANCELED/!+NEXT"
                       ((org-agenda-overriding-header "Next Actions")))
            (tags-todo "-CANCELED/!+WAITING"
                       ((org-agenda-overriding-header "Waiting for")
                        (org-tags-match-list-sublevels nil)))
            (tags-todo "+PROJECT-WAITING-CANCELED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels nil)))
            (tags-todo "+SOMEDAY-CANCELED/!"
                       ((org-agenda-overriding-header "Someday/Maybe")
                        (org-tags-match-list-sublevels nil))))))))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Task" entry
           (file "~/org/inbox.org")
           "* TODO %?\n  %U")
          ("n" "Note" entry
           (file "~/org/inbox.org")
           "* %? :note:\n  %U"))))

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
  :bind-keymap ("C-c p" . projectile-command-map)
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
  :bind ("C-s" . swiper))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

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

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;; init.el ends here
