;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

(eval-and-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (leaf-keywords-init))

(leaf *better-defaults
  :custom
  (auto-save-default       . nil)
  (blink-cursor-mode       . nil)
  (column-number-mode      . t)
  (confirm-kill-emacs      . 'yes-or-no-p)
  (create-lockfiles        . nil)
  (indent-tabs-mode        . nil)
  (make-backup-files       . nil)
  (recentf-max-saved-items . 1024)
  (require-final-newline   . t)
  (scroll-bar-mode         . nil)
  (scroll-conservatively   . 10000)
  (tab-width               . 2)
  (tool-bar-mode           . nil))

(leaf *fonts
  :when window-system
  :config
  (cond
   ((eq window-system 'x)
    (set-face-attribute 'default nil :family "Ricty" :height 120))
   ((eq window-system 'ns)
    (set-face-attribute 'default nil :family "Ricty" :height 140))))

(leaf autorevert
  :custom
  (global-auto-revert-mode . t))

(leaf elec-pair
  :custom
  (electric-pair-mode . t))

(leaf flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(leaf hideshow
  :hook
  (prog-mode . hs-minor-mode))

(leaf hl-line
  :custom
  (global-hl-line-mode . t))

(leaf org
  :custom
  (org-agenda-files                   . '("~/Dropbox/org/inbox.org"
                                          "~/Dropbox/org/gtd.org"
                                          "~/Dropbox/org/tickler.org"))
  (org-agenda-text-search-extra-files . '(agenda-archives))
  (org-babel-load-languages           . '((emacs-lisp . t)
                                          (shell      . t)))
  (org-capture-templates              . '(("t" "Task" entry
                                           (file "~/Dropbox/org/inbox.org")
                                           "* TODO %?\n%U\n%a"
                                           :prepend t)
                                          ("n" "Note" entry
                                           (file "~/Dropbox/org/inbox.org")
                                           "* %?\n%U\n%a"
                                           :prepend t)))
  (org-edit-src-content-indentation   . 0)
  (org-log-done                       . 'time)
  (org-log-into-drawer                . t)
  (org-modules                        . '(org-docview org-habit org-info))
  (org-outline-path-complete-in-steps . nil)
  (org-refile-targets                 . '(("~/Dropbox/org/gtd.org"     :maxlevel . 3)
                                          ("~/Dropbox/org/someday.org" :level    . 1)
                                          ("~/Dropbox/org/tickler.org" :maxlevel . 3)))
  (org-refile-use-outline-path        . 'file)
  (org-reverse-note-order             . t)
  (org-src-window-setup               . 'current-window)
  (org-tag-alist                      . '(("@office" . ?o)
                                          ("@home"   . ?h)))
  (org-todo-keywords                  . '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))
  (org-use-speed-commands             . t)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :config
  (setq system-time-locale "C"))

(leaf paren
  :custom
  (show-paren-mode . t))

(leaf ruby-mode
  :custom
  (ruby-insert-encoding-magic-comment . nil))

(leaf sh-script
  :custom
  (sh-basic-offset . 2))

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf company
  :straight t
  :custom
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-show-numbers          . t)
  (global-company-mode           . t)
  :config
  (leaf company-quickhelp
    :straight t
    :custom
    (company-quickhelp-mode . t)))

(leaf docker-compose-mode
  :straight t)

(leaf dockerfile-mode
  :straight t)

(leaf edit-server
  :straight t
  :custom
  (edit-server-new-frame . nil))

(leaf exec-path-from-shell
  :when window-system
  :straight t
  :defun
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

(leaf expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

(leaf flycheck
  :straight t
  :custom
  (global-flycheck-mode . t))

(leaf git-timemachine
  :straight t)

(leaf helpful
  :straight t
  :bind
  ("C-h k" . helpful-key))

(leaf ivy
  :straight t
  :custom
  (ivy-mode                . t)
  (ivy-use-virtual-buffers . t)
  (ivy-virtual-abbreviate  . 'abbreviate)
  :bind
  ("C-c C-r" . ivy-resume)
  :config
  (leaf counsel
    :straight t
    :custom
    (counsel-describe-function-function . 'helpful-callable)
    (counsel-describe-variable-function . 'helpful-variable)
    (counsel-mode . t))

  (leaf swiper
    :straight t
    :bind
    ([remap isearch-forward] . swiper))

  (leaf ivy-hydra
    :straight t)

  (leaf ivy-rich
    :straight t
    :custom
    (ivy-rich-mode . t))

  (leaf smex
    :straight t))

(leaf json-mode
  :straight t)

(leaf ledger-mode
  :straight t
  :custom
  (ledger-post-amount-alignment-column . 65)
  (ledger-reports
   . '(("Balance Sheet"
        "%(binary) bal -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本")
       ("Monthly Balance"
        "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --monthly --collapse")
       ("Monthly Expence"
        "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --monthly --sort -amount")
       ("Account Statement"
        "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)")))
  :config
  (leaf flycheck-ledger
    :after flycheck
    :straight t
    :require t))

(leaf magit
  :straight t
  :bind
  ("C-c m" . magit-status))

(leaf markdown-mode
  :straight t
  :custom
  (markdown-fontify-code-block-natively . t)
  :custom-face
  (markdown-code-face . '((t (:inherit default)))))

(leaf minions
  :straight t
  :custom
  (minions-direct . '(flycheck-mode))
  (minions-mode   . t))

(leaf mozc
  :when (eq window-system 'x)
  :straight t
  :custom
  (default-input-method . "japanese-mozc")
  :config
  (leaf mozc-popup
    :straight t
    :require t
    :custom
    (mozc-candidate-style . 'popup)))

(leaf php-mode
  :straight t)

(leaf projectile
  :straight t
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-mode              . t)
  :bind
  ("C-c p" . projectile-command-map))

(leaf rg
  :straight t
  :bind
  ("C-c s" . rg-menu))

(leaf solarized-theme
  :when window-system
  :straight t
  :custom
  (solarized-scale-org-headlines . nil)
  (solarized-use-variable-pitch  . nil)
  (x-underline-at-descent-line   . t)
  :config
  (load-theme 'solarized-dark t))

(leaf undo-tree
  :straight t
  :custom
  (global-undo-tree-mode . t))

(leaf which-key
  :straight t
  :custom
  (which-key-mode . t))

(leaf yasnippet
  :straight t
  :custom
  (yas-global-mode . t)
  :config
  (leaf yasnippet-snippets
    :straight t))

(provide 'init)
;;; init.el ends here
