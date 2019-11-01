;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

(progn ;     patch
  ;; https://github.com/bbatsov/prelude/issues/1225
  (custom-set-variables
   '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

(progn ;     startup
  (custom-set-variables
   '(auto-save-default nil)
   '(blink-cursor-mode nil)
   '(column-number-mode t)
   '(confirm-kill-emacs 'yes-or-no-p)
   '(create-lockfiles nil)
   '(custom-file (expand-file-name "custom.el" user-emacs-directory))
   '(indent-tabs-mode nil)
   '(make-backup-files nil)
   '(require-final-newline t)
   '(scroll-bar-mode nil)
   '(scroll-conservatively 10000)
   '(tab-width 2)
   '(tool-bar-mode nil))

  ;; font
  (when window-system
    (set-face-attribute 'default nil :family "Ricty" :height 120)
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty")))

  ;; local.el
  (let ((local-file (expand-file-name "local.el" user-emacs-directory)))
    (load local-file t)))

(progn ;    `package'
  (custom-set-variables
   '(package-archives
     '(("gnu" . "https://elpa.gnu.org/packages/")
       ("melpa" . "https://melpa.org/packages/"))))
  (package-initialize))

(progn ;    `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (custom-set-variables
   '(use-package-enable-imenu-support t)
   '(use-package-verbose t)))

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package company
  :ensure
  :custom
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (global-company-mode t))

(use-package company-quickhelp
  :ensure
  :after company
  :custom
  (company-quickhelp-mode t))

(use-package counsel
  :ensure
  :after ivy
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable)
  (counsel-mode t))

(use-package docker
  :ensure
  :bind
  ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure
  :defer)

(use-package docker-tramp
  :ensure
  :defer
  :custom
  (docker-tramp-use-names t))

(use-package dockerfile-mode
  :ensure
  :defer)

(use-package edit-server
  :ensure
  :custom
  (edit-server-new-frame nil)
  :config
  (edit-server-start))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package exec-path-from-shell
  :ensure
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :ensure
  :custom
  (global-flycheck-mode t))

(use-package flycheck-ledger
  :ensure
  :after (flycheck ledger-mode))

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(use-package helpful
  :ensure
  :bind
  ("C-h k" . helpful-key))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode))

(use-package hl-line
  :custom
  (global-hl-line-mode t))

(use-package ivy
  :ensure
  :custom
  (ivy-mode t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate))

(use-package ivy-rich
  :ensure
  :after ivy
  :custom
  (ivy-rich-mode t))

(use-package json-mode
  :ensure
  :defer)

(use-package ledger-mode
  :ensure
  :defer
  :custom
  (ledger-post-amount-alignment-column 65)
  (ledger-reports
   '(("Balance Sheet"
      "%(binary) bal -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本")
     ("Monthly Balance"
      "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --monthly --collapse")
     ("Monthly Expence"
      "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --monthly --sort -amount")
     ("Account Statement"
      "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)"))))

(use-package magit
  :ensure
  :bind
  ("C-c m" . magit-status))

(use-package markdown-mode
  :ensure
  :defer
  :custom
  (markdown-code-lang-modes
   '(("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("ruby" . ruby-mode)))
  (markdown-fontify-code-blocks-natively t)
  :custom-face
  (markdown-code-face ((t (:inherit default :foreground "#839496")))))

(use-package minions
  :ensure
  :custom
  (minions-direct '(flycheck-mode))
  (minions-mode t))

(use-package mozc
  :if (memq window-system '(x))
  :custom
  (default-input-method "japanese-mozc"))

(use-package mozc-popup
  :ensure
  :if (memq window-system '(x))
  :after mozc
  :custom
  (mozc-candidate-style 'popup))

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :custom
  (org-agenda-files
   '("~/Dropbox/org/inbox.org"
     "~/Dropbox/org/gtd.org"
     "~/Dropbox/org/tickler.org"))
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (org-capture-templates
   '(("t" "Task" entry
      (file "~/Dropbox/org/inbox.org")
      "* TODO %?\n%U\n%a"
      :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/org/inbox.org")
      "* %?\n%U\n%a"
      :prepend t)))
  (org-edit-src-content-indentation 0)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-modules '(org-docview org-habit org-info))
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets
   '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
     ("~/Dropbox/org/someday.org" :level . 1)
     ("~/Dropbox/org/tickler.org" :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-reverse-note-order t)
  (org-src-window-setup 'current-window)
  (org-tag-alist
   '(("@office" . ?o)
     ("@home" . ?h)))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))
  (org-use-speed-commands t)
  :config
  (setq system-time-locale "C"))

(use-package paradox
  :ensure
  :defer
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t)
  :custom-face
  (paradox-mode-line-face ((t nil))))

(use-package paren
  :custom
  (show-paren-mode t))

(use-package php-mode
  :ensure
  :defer)

(use-package projectile
  :ensure
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ruby-mode
  :defer
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package smex
  :ensure
  :defer)

(use-package solarized-theme
  :ensure
  :if window-system
  :custom
  (solarized-high-contrast-mode-line t)
  (solarized-scale-org-headlines nil)
  (solarized-use-variable-pitch nil)
  :config
  (load-theme 'solarized-dark t))

(use-package swiper
  :ensure
  :after ivy
  :bind
  ("C-s" . swiper))

(use-package undo-tree
  :ensure t
  :custom
  (global-undo-tree-mode t))

(use-package web-mode
  :ensure
  :mode
  ("\\.erb\\'")
  :custom
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :ensure
  :custom
  (which-key-mode t))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yasnippet
  :ensure
  :custom
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure
  :after yasnippet)

(provide 'init)
;;; init.el ends here
