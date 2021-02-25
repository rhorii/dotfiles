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

(leaf *mac
  :when (memq window-system '(ns mac))
  :custom
  (mac-auto-ascii-mode . t)
  (mac-command-modifier . 'super)
  (mac-option-modifier . 'meta)
  :config
  (set-face-attribute 'default nil :family "HackGenNerd" :height 130)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "HackGenNerd")))

(leaf *linux
  :when (memq window-system '(x pgtk))
  :config
  (set-face-attribute 'default nil :family "Ricty" :height 120))

(leaf autorevert
  :global-minor-mode global-auto-revert-mode
  :custom
  (auto-revert-check-vc-info . nil))

(leaf cus-edit
  :custom
  `(custom-file . ,(locate-user-emacs-file "custom.el")))

(leaf cus-start
  :custom
  (create-lockfiles . nil)
  (frame-resize-pixelwise . t)
  (history-delete-duplicates . t)
  (history-length . t)
  (indent-tabs-mode . nil)
  (scroll-conservatively . 100)
  (tab-width . 2)
  (tool-bar-mode . nil))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf files
  :custom
  `(auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
  `(backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                               (,tramp-file-name-regexp . nil)))
  (confirm-kill-emacs . 'yes-or-no-p)
  (delete-old-versions . t)
  (require-final-newline . t)
  (version-control . t))

(leaf flyspell
  :hook
  (text-mode-hook)
  (prog-mode-hook . flyspell-prog-mode))

(leaf frame
  :bind
  ("C-s-f" . toggle-frame-fullscreen)
  :custom
  (blink-cursor-mode . nil))

(leaf hideshow
  :hook
  (prog-mode-hook . hs-minor-mode))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf recentf
  :global-minor-mode t
  :custom
  (recentf-max-saved-items . 1024))

(leaf ruby-mode
  :custom
  (ruby-insert-encoding-magic-comment . nil))

(leaf savehist
  :global-minor-mode t)

(leaf scroll-bar
  :custom
  (scroll-bar-mode . nil))

(leaf sh-script
  :custom
  (sh-basic-offset . 2))

(leaf simple
  :global-minor-mode column-number-mode)

(leaf startup
  :custom
  `(auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-")))

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf company
  :ensure t
  :global-minor-mode global-company-mode
  :custom
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-show-numbers . t)
  :config
  (leaf company-quickhelp
    :ensure t
    :global-minor-mode t))

(leaf docker
  :ensure t
  :bind
  ("C-c d" . docker))

(leaf docker-compose-mode :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf edit-server
  :ensure t
  :custom
  (edit-server-new-frame . nil))

(leaf exec-path-from-shell
  :ensure t
  :when window-system
  :config
  (exec-path-from-shell-initialize))

(leaf expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(leaf flycheck
  :ensure t
  :hook
  (prog-mode-hook))

(leaf git-timemachine :ensure t)

(leaf google-translate
  :ensure t
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :custom
  (google-translate-pop-up-buffer-set-focus . t)
  (google-translate-translation-directions-alist . '(("en" . "ja")
                                                     ("ja" . "en"))))

(leaf helpful
  :ensure t
  :bind
  ("C-h k" . helpful-key))

(leaf ivy
  :ensure t
  :leaf-defer nil
  :global-minor-mode t
  :bind
  ("C-c C-r" . ivy-resume)
  :custom
  (ivy-height . 24)
  (ivy-use-virtual-buffers . nil)
  (ivy-virtual-abbreviate . 'abbreviate)
  :config
  (leaf counsel
    :ensure t
    :global-minor-mode t
    :defun with-ivy-window
    :defvar recentf-list
    :bind
    ("C-c m" . counsel-mark-ring)
    ("C-c i" . counsel-imenu)
    ("C-x b" . counsel-switch-buffer)
    ("C-x C-b" . counsel-ibuffer)
    ("C-x C-r" . counsel-recentf)
    :preface
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (mapcar (lambda (x) (abbreviate-file-name  ;; ~/
                                     (substring-no-properties x)))
                        recentf-list)
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    :advice
    (:override counsel-recentf ad:counsel-recentf)
    :custom
    (counsel-describe-function-function . 'helpful-callable)
    (counsel-describe-variable-function . 'helpful-variable))

  (leaf swiper
    :ensure t
    :bind
    ("C-s" . swiper))

  (leaf ivy-rich
    :ensure t
    :global-minor-mode t))

(leaf json-mode :ensure t)

(leaf ledger-mode
  :ensure t
  :custom
  (ledger-post-amount-alignment-column . 65)
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
        "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)")))
  :config
  (leaf flycheck-ledger
    :ensure t
    :hook
    (ledger-mode-hook . flycheck-mode)
    :require t))

(leaf magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-completing-read-function . 'ivy-completing-read)
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-block-natively . t)
  :custom-face
  (markdown-code-face . '((t (:inherit default)))))

(leaf migemo
  :ensure t
  :when (executable-find "cmigemo")
  :require t
  :defun migemo-get-pattern migemo-init
  :defvar ivy-re-builders-alist
  :preface
  (defun ivy-migemo-re-builder (str)
    (car (seq-reduce (lambda (acc char)
                       (let* ((regex (car acc))
                              (plain (cdr acc))
                              (s (char-to-string char))
                              (sp (cond ((eq #x3 char) "")
                                        ((eq ?  char) ".*")
                                        (t s))))
                         (if (seq-contains-p (concat " .+?[]^$\\" (char-to-string #x3)) char)
                             (cons (concat regex (migemo-get-pattern plain) sp) nil)
                           (cons regex (concat plain s)))))
                     (concat str (char-to-string #x3))
                     '("" . ""))))
  :custom
  (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy-migemo-re-builder))
  (migemo-init))

(leaf minions
  :ensure t
  :global-minor-mode t
  :custom
  (minions-direct . '(flycheck-mode)))

(leaf mozc
  :ensure t
  :when (memq window-system '(x pgtk))
  :custom
  (default-input-method . "japanese-mozc")
  :config
  (leaf mozc-popup
    :ensure t
    :require t
    :custom
    (mozc-candidate-style . 'popup)))

(leaf org
  :ensure t
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :custom
  (org-agenda-files . '("~/org/inbox.org"
                        "~/org/gtd.org"
                        "~/org/tickler.org"))
  (org-agenda-text-search-extra-files . '(agenda-archives))
  (org-babel-load-languages . '((emacs-lisp . t)
                                (shell . t)))
  (org-capture-templates . '(("t" "Task" entry
                              (file "~/org/inbox.org")
                              "* TODO %?\n%U\n%a"
                              :prepend t)
                             ("n" "Note" entry
                              (file "~/org/inbox.org")
                              "* %?\n%U\n%a"
                              :prepend t)))
  (org-edit-src-content-indentation . 0)
  (org-log-done . 'time)
  (org-log-into-drawer . t)
  (org-modules . '(org-docview org-habit org-info))
  (org-outline-path-complete-in-steps . nil)
  (org-refile-targets . '(("~/org/gtd.org" :maxlevel . 3)
                          ("~/org/someday.org" :level . 1)
                          ("~/org/tickler.org" :maxlevel . 3)))
  (org-refile-use-outline-path . 'file)
  (org-replace-disputed-keys . t)
  (org-reverse-note-order . t)
  (org-src-window-setup . 'current-window)
  (org-startup-folded . 'content)
  (org-startup-indented . t)
  (org-tag-alist . '(("@office" . ?o)
                     ("@home" . ?h)))
  (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))
  (org-use-speed-commands . t)
  :config
  (setq system-time-locale "C")
  (leaf ob-async
    :ensure t
    :require t))

(leaf php-mode :ensure t)

(leaf pocket-reader :ensure t)

(leaf projectile
  :ensure t
  :global-minor-mode t
  :bind
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map)
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-enable-caching . t)
  :config
  (leaf counsel-projectile
    :ensure t
    :after counsel
    :global-minor-mode t))

(leaf rg
  :ensure t
  :bind
  ("C-c s r" . rg-menu))

(leaf smex :ensure t)

(leaf solarized-theme
  :ensure t
  :when window-system
  :custom
  (solarized-scale-org-headlines . nil)
  (solarized-use-variable-pitch . nil)
  (x-underline-at-descent-line . t)
  :config
  (load-theme 'solarized-dark t))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode)

(leaf web-mode
  :ensure t
  :mode "\\.x[ms]l\\'"
  :custom
  (web-mode-script-padding . 4))

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf yasnippet
  :ensure t
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (leaf yasnippet-snippets :ensure t))

(leaf zeal-at-point
  :ensure t
  :when (memq window-system '(x pgtk))
  :bind
  ("C-c z" . zeal-at-point))

(provide 'init)
;;; init.el ends here
