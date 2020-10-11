;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))

(leaf *fonts
  :when window-system
  :config
  (cond
   ((memq window-system '(x pgtk))
    (set-face-attribute 'default nil :family "Ricty" :height 120))
   ((eq window-system 'ns)
    (set-face-attribute 'default nil :family "Ricty" :height 140))))

(leaf autorevert
  :custom ((auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)

(leaf cus-edit
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :custom ((create-lockfiles . nil)
           (indent-tabs-mode . nil)
           (scroll-conservatively . 100)
           (tab-width . 2)
           (tool-bar-mode . nil)))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf files
  :custom `((auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (confirm-kill-emacs . 'yes-or-no-p)
            (delete-old-versions . t)
            (require-final-newline . t)
            (version-control . t)))

(leaf flyspell
  :hook (text-mode-hook (prog-mode-hook . flyspell-prog-mode)))

(leaf frame
  :custom ((blink-cursor-mode . nil)))

(leaf hideshow
  :hook ((prog-mode-hook . hs-minor-mode)))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf recentf
  :custom ((recentf-max-saved-items . 1024)))

(leaf ruby-mode
  :custom ((ruby-insert-encoding-magic-comment . nil)))

(leaf scroll-bar
  :custom (scroll-bar-mode . nil))

(leaf sh-script
  :custom ((sh-basic-offset . 2)))

(leaf simple
  :global-minor-mode column-number-mode)

(leaf startup
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf company
  :ensure t
  :custom ((company-idel-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-show-numbers . t))
  :global-minor-mode global-company-mode)

(leaf company-quickhelp
  :ensure t
  :after company
  :global-minor-mode t)

(leaf counsel-projectile
  :ensure t
  :after counsel projectile
  :global-minor-mode t)

(leaf docker
  :ensure t
  :bind (("C-c d" . docker)))

(leaf docker-compose-mode :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf edit-server
  :ensure t
  :custom ((edit-server-new-frame . nil)))

(leaf exec-path-from-shell
  :when window-system
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(leaf flycheck
  :ensure t
  :hook prog-mode-hook ledger-mode-hook)

(leaf git-timemachine :ensure t)

(leaf google-translate
  :ensure t
  :bind (("C-c t" . google-translate-smooth-translate))
  :custom ((google-translate-pop-up-buffer-set-focus . t)
           (google-translate-translation-directions-alist . '(("en" . "ja")
                                                              ("ja" . "en")))))

(leaf helpful
  :ensure t
  :bind (("C-h k" . helpful-key)))

(leaf ivy
  :ensure t
  :leaf-defer nil
  :bind (("C-c C-r" . ivy-resume))
  :custom ((ivy-use-virtual-buffers . nil)
           (ivy-virtual-abbreviate . 'abbreviate))
  :global-minor-mode t
  :config
  (leaf counsel
    :ensure t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom ((counsel-describe-function-function . 'helpful-callable)
             (counsel-describe-variable-function . 'helpful-variable))
    :global-minor-mode t)

  (leaf swiper
    :ensure t
    :bind (("C-s" . swiper))))

(leaf ivy-rich
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf json-mode :ensure t)

(leaf ledger-mode
  :ensure t
  :custom ((ledger-post-amount-alignment-column . 65)
           (ledger-reports
            . '(("Balance Sheet"
                 "%(binary) bal -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本")
                ("Monthly Balance"
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --monthly --collapse")
                ("Monthly Expence"
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --monthly --sort -amount")
                ("Yearly Balance"
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --yearly --collapse")
                ("Yearly Expence"
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --yearly --sort -amount")
                ("Account Statement"
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)"))))
  :config
  (leaf flycheck-ledger
    :after flycheck
    :ensure t
    :require t))

(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(leaf markdown-mode
  :ensure t
  :custom ((markdown-fontify-code-block-natively . t))
  :custom-face ((markdown-code-face . '((t (:inherit default))))))

(leaf migemo
  :when (executable-find "cmigemo")
  :ensure t
  :defun migemo-init
  :custom ((migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))
  :require t
  :config
  (migemo-init))

(leaf minions
  :ensure t
  :custom ((minions-direct . '(flycheck-mode)))
  :global-minor-mode t)

(leaf mozc
  :when (memq window-system '(x pgtk))
  :ensure t
  :custom ((default-input-method . "japanese-mozc")))

(leaf mozc-popup
  :ensure t
  :after mozc
  :custom ((mozc-candidate-style . 'popup))
  :require t)

(leaf org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom ((org-agenda-files . '("~/Dropbox/org/inbox.org"
                                 "~/Dropbox/org/gtd.org"
                                 "~/Dropbox/org/tickler.org"))
           (org-agenda-text-search-extra-files . '(agenda-archives))
           (org-babel-load-languages . '((emacs-lisp . t)
                                         (shell . t)))
           (org-capture-templates . '(("t" "Task" entry
                                       (file "~/Dropbox/org/inbox.org")
                                       "* TODO %?\n%U\n%a"
                                       :prepend t)
                                      ("n" "Note" entry
                                       (file "~/Dropbox/org/inbox.org")
                                       "* %?\n%U\n%a"
                                       :prepend t)))
           (org-edit-src-content-indentation . 0)
           (org-log-done . 'time)
           (org-log-into-drawer . t)
           (org-modules . '(org-docview org-habit org-info))
           (org-outline-path-complete-in-steps . nil)
           (org-refile-targets . '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
                                   ("~/Dropbox/org/someday.org" :level . 1)
                                   ("~/Dropbox/org/tickler.org" :maxlevel . 3)))
           (org-refile-use-outline-path . 'file)
           (org-replace-disputed-keys . t)
           (org-reverse-note-order . t)
           (org-src-window-setup . 'current-window)
           (org-startup-folded . 'content)
           (org-startup-indented . t)
           (org-tag-alist . '(("@office" . ?o)
                              ("@home" . ?h)))
           (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))
           (org-use-speed-commands . t))
  :config
  (setq system-time-locale "C")
  (leaf ob-async
    :ensure t
    :require t))

(leaf php-mode :ensure t)

(leaf pocket-reader :ensure t)

(leaf projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :custom ((projectile-completion-system . 'ivy))
  :global-minor-mode t)

(leaf rg
  :ensure t
  :bind (("C-c s" . rg-menu)))

(leaf smex :ensure t)

(leaf solarized-theme
  :when window-system
  :ensure t
  :custom ((solarized-scale-org-headlines . nil)
           (solarized-use-variable-pitch . nil)
           (x-underline-at-descent-line . t))
  :config
  (load-theme 'solarized-dark t))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode)

(leaf web-mode
  :ensure t
  :mode "\\.x[ms]l\\'")

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf yasnippet
  :ensure t
  :hook ((prog-mode-hook . yas-minor-mode)))

(leaf yasnippet-snippets
  :ensure t
  :after yasnippet)

(leaf zeal-at-point
  :when (memq window-system '(x pgtk))
  :ensure t
  :bind (("C-c z" . zeal-at-point)))

(provide 'init)
;;; init.el ends here
