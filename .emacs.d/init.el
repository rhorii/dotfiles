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
    :init
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf general
  :bind ("C-s-f" . toggle-frame-fullscreen)
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))
            (create-lockfiles . nil)
            (frame-resize-pixelwise . t)
            (history-delete-duplicates . t)
            (history-length . t)
            (indent-tabs-mode . nil)
            (scroll-conservatively . 100)
            (tab-width . 2)
            (tool-bar-mode . nil)
            ;; files
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (confirm-kill-emacs . 'yes-or-no-p)
            (delete-old-versions . t)
            (require-final-newline . t)
            (version-control . t)
            ;; frames
            (blink-cursor-mode . nil)
            ;; scroll-bar
            (scroll-bar-mode . nil)
            ;; simple
            (column-number-mode . t)
            ;; startup
            (auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf *mac
  :when (memq window-system '(ns mac))
  :custom ((mac-auto-ascii-mode . t)
           (mac-command-modifier . 'super)
           (mac-option-modifier . 'meta))
  :config
  (set-face-attribute 'default nil :family "HackGenNerd" :height 130)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "HackGenNerd")))

(leaf *linux
  :when (memq window-system '(x pgtk))
  :config
  (set-face-attribute 'default nil :family "Ricty" :height 120))

(leaf autorevert
  :custom (auto-revert-check-vc-info . nil)
  :global-minor-mode global-auto-revert-mode)

(leaf company
  :package t
  :blackout t
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-show-numbers . t))
  :global-minor-mode global-company-mode)

(leaf company-statistics
  :package t
  :after company
  :global-minor-mode t)

(leaf company-quickhelp
  :package t
  :after company
  :global-minor-mode t)

(leaf counsel
  :package t
  :blackout t
  :after ivy
  :defun with-ivy-window
  :defvar recentf-list
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
  :bind (("C-c m" . counsel-mark-ring)
         ("C-c i" . counsel-imenu)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-r" . counsel-recentf))
  :advice (:override counsel-recentf ad:counsel-recentf)
  :custom ((counsel-describe-function-function . 'helpful-callable)
           (counsel-describe-variable-function . 'helpful-variable))
  :global-minor-mode t)

(leaf counsel-projectile
  :package t
  :after counsel projectile
  :global-minor-mode t)

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf docker
  :package t
  :bind ("C-c d" . docker))

(leaf docker-compose-mode :package t)

(leaf dockerfile-mode :package t)

(leaf edit-server
  :package t
  :custom (edit-server-new-frame . nil))

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf exec-path-from-shell
  :when window-system
  :package t
  :config
  (exec-path-from-shell-initialize))

(leaf expand-region
  :package t
  :bind ("C-=" . er/expand-region))

(leaf flycheck
  :package t
  :global-minor-mode global-flycheck-mode)

(leaf flycheck-color-mode-line
  :package t
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

(leaf flycheck-ledger
  :package t
  :after flycheck
  :require t)

(leaf flyspell
  :blackout t
  :hook ((text-mode-hook)
         (prog-mode-hook . flyspell-prog-mode)))

(leaf git-timemachine :package t)

(leaf google-translate
  :package t
  :bind ("C-c t" . google-translate-smooth-translate)
  :preface
  (defun ad:google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :advice (:override google-translate--search-tkk ad:google-translate--search-tkk)
  :custom ((google-translate-backend-method . 'curl)
           (google-translate-pop-up-buffer-set-focus . t)
           (google-translate-translation-directions-alist . '(("en" . "ja")
                                                              ("ja" . "en")))))

(leaf helpful
  :package t
  :bind ("C-h k" . helpful-key))

(leaf hideshow
  :blackout hs-minor-mode
  :hook (prog-mode-hook . hs-minor-mode))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf ivy
  :package t
  :blackout t
  :bind ("C-c C-r" . ivy-resume)
  :custom ((ivy-height . 24)
           (ivy-use-virtual-buffers . nil)
           (ivy-virtual-abbreviate . 'abbreviate))
  :global-minor-mode t)

(leaf ivy-rich
  :package t
  :after ivy
  :global-minor-mode t)

(leaf json-mode :package t)

(leaf ledger-mode
  :package t
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
                 "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)")))))

(leaf magit
  :package t
  :bind ("C-x g" . magit-status)
  :custom ((magit-completing-read-function . 'ivy-completing-read)
           (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1)))

(leaf markdown-mode
  :package t
  :custom (markdown-fontify-code-block-natively . t)
  :custom-face (markdown-code-face . '((t (:inherit default)))))

(leaf migemo
  :when (executable-find "cmigemo")
  :package t
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
  :custom (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy-migemo-re-builder))
  (migemo-init))

(leaf minions
  :disabled t
  :package t
  :custom (minions-direct . '(flycheck-mode))
  :global-minor-mode t)

(leaf mozc
  :when (memq window-system '(x pgtk))
  :package t
  :custom (default-input-method . "japanese-mozc"))

(leaf mozc-popup
  :when (memq window-system '(x pgtk))
  :package t
  :after mozc
  :require t
  :custom (mozc-candidate-style . 'popup))

(leaf org
  :package t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom ((org-agenda-files . '("~/org/inbox.org"
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
           (org-use-speed-commands . t))
  :config
  (setq system-time-locale "C")
  (leaf ob-async
    :package t
    :require t))

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf php-mode :package t)

(leaf pocket-reader :package t)

(leaf projectile
  :package t
  :bind (("C-c p" . projectile-command-map)
         ("s-p" . projectile-command-map))
  :custom ((projectile-completion-system . 'ivy)
           (projectile-enable-caching . t))
  :global-minor-mode t)

(leaf recentf
  :custom (recentf-max-saved-items . nil)
  :global-minor-mode t)

(leaf rg
  :package t
  :bind ("C-c s r" . rg-menu))

(leaf ruby-mode
  :custom (ruby-insert-encoding-magic-comment . nil))

(leaf savehist
  :custom ((savehist-additional-variables . '(projectile-project-command-history)))
  :global-minor-mode t)

(leaf sh-script
  :custom (sh-basic-offset . 2))

(leaf shackle
  :package t
  :custom ((shackle-rules . '((compilation-mode :select t)
                              ("\\*Async Shell.*\\*" :regexp t :popup t :align below :size 0.3))))
  :global-minor-mode t)

(leaf smex :package t)

(leaf solarized-theme
  :when window-system
  :package t
  :custom ((solarized-scale-org-headlines . nil)
           (solarized-use-variable-pitch . nil)
           (x-underline-at-descent-line . t))
  :config
  (load-theme 'solarized-dark t))

(leaf swiper
  :package t
  :after ivy
  :bind ("C-s" . swiper))

(leaf undo-tree
  :package t
  :blackout t
  :global-minor-mode global-undo-tree-mode)

(leaf uniquify
  :custom (uniquify-buffer-name-style . 'forward))

(leaf web-mode
  :package t
  :mode "\\.x[ms]l\\'"
  :custom (web-mode-script-padding . 4))

(leaf which-key
  :package t
  :blackout t
  :global-minor-mode t)

(leaf windmove
  :custom (windmove-wrap-around . t)
  :config
  (windmove-default-keybindings))

(leaf yasnippet
  :package t
  :blackout yas-minor-mode
  :hook (prog-mode-hook . yas-minor-mode))

(leaf yasnippet-snippets
  :package t
  :after yasnippet)

(leaf zeal-at-point
  :when (memq window-system '(x pgtk))
  :package t
  :bind ("C-c z" . zeal-at-point))

(provide 'init)
;;; init.el ends here
