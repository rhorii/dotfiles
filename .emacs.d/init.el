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
  (prog-mode-hook . flyspell-prog-mode)
  (text-mode-hook . flyspell-mode))

(leaf hideshow
  :hook
  (prog-mode-hook . hs-minor-mode))

(leaf hl-line
  :custom
  (global-hl-line-mode . t))

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
  :ensure t
  :custom
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-show-numbers          . t)
  (global-company-mode           . t)
  :config
  (leaf company-quickhelp
    :ensure t
    :custom
    (company-quickhelp-mode . t)))

(leaf docker-compose-mode
  :ensure t)

(leaf dockerfile-mode
  :ensure t)

(leaf edit-server
  :ensure t
  :custom
  (edit-server-new-frame . nil))

(leaf exec-path-from-shell
  :when window-system
  :ensure t
  :defun
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

(leaf expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(leaf flycheck
  :ensure t
  :custom
  (global-flycheck-mode . t))

(leaf git-timemachine
  :ensure t)

(leaf google-translate
  :ensure t
  :custom
  (google-translate-pop-up-buffer-set-focus . t)
  (google-translate-translation-directions-alist . '(("en" . "ja")
                                                     ("ja" . "en")))
  :bind
  ("C-c t" . google-translate-smooth-translate))

(leaf helpful
  :ensure t
  :bind
  ("C-h k" . helpful-key))

(leaf ivy
  :ensure t
  :custom
  (ivy-mode                . t)
  (ivy-use-virtual-buffers . t)
  (ivy-virtual-abbreviate  . 'abbreviate)
  :bind
  ("C-c C-r" . ivy-resume)
  :config
  (leaf counsel
    :ensure t
    :custom
    (counsel-describe-function-function . 'helpful-callable)
    (counsel-describe-variable-function . 'helpful-variable)
    (counsel-mode . t))

  (leaf swiper
    :ensure t
    :bind
    ([remap isearch-forward] . swiper))

  (leaf ivy-hydra
    :ensure t)

  (leaf ivy-rich
    :ensure t
    :custom
    (ivy-rich-mode . t))

  (leaf smex
    :ensure t))

(leaf json-mode
  :ensure t)

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
    :after flycheck
    :ensure t
    :require t))

(leaf magit
  :ensure t
  :bind
  ("C-c m" . magit-status))

(leaf markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-block-natively . t)
  :custom-face
  (markdown-code-face . '((t (:inherit default)))))

(leaf minions
  :ensure t
  :custom
  (minions-direct . '(flycheck-mode))
  (minions-mode   . t))

(leaf mozc
  :when (eq window-system 'x)
  :ensure t
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
  (org-replace-disputed-keys          . t)
  (org-reverse-note-order             . t)
  (org-src-window-setup               . 'current-window)
  (org-startup-folded                 . 'content)
  (org-startup-indented               . t)
  (org-tag-alist                      . '(("@office" . ?o)
                                          ("@home"   . ?h)))
  (org-todo-keywords                  . '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")))
  (org-use-speed-commands             . t)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :config
  (setq system-time-locale "C")

  (leaf ob-async
    :ensure t
    :require t))

(leaf php-mode
  :ensure t)

(leaf pocket-reader
  :ensure t)

(leaf projectile
  :ensure t
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-mode              . t)
  :bind
  ("C-c p" . projectile-command-map))

(leaf rg
  :ensure t
  :bind
  ("C-c s" . rg-menu))

(leaf solarized-theme
  :when window-system
  :ensure t
  :custom
  (solarized-scale-org-headlines . nil)
  (solarized-use-variable-pitch  . nil)
  (x-underline-at-descent-line   . t)
  :config
  (load-theme 'solarized-dark t))

(leaf undo-tree
  :ensure t
  :custom
  (global-undo-tree-mode . t))

(leaf web-mode
  :ensure t
  :mode "\\.x[ms]l\\'")

(leaf which-key
  :ensure t
  :custom
  (which-key-mode . t))

(leaf yasnippet
  :ensure t
  :custom
  (yas-global-mode . t)
  :config
  (leaf yasnippet-snippets
    :ensure t))

(leaf zeal-at-point
  :when (eq window-system 'x)
  :ensure t
  :bind
  ("C-c d" . zeal-at-point))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-mode t)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(counsel-describe-function-function 'helpful-callable)
 '(counsel-describe-variable-function 'helpful-variable)
 '(counsel-mode t)
 '(create-lockfiles nil)
 '(default-input-method "japanese-mozc")
 '(edit-server-new-frame nil t)
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(google-translate-pop-up-buffer-set-focus t t)
 '(google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")) t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-rich-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'abbreviate)
 '(ledger-post-amount-alignment-column 65 t)
 '(ledger-reports
   '(("Balance Sheet" "%(binary) bal -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本")
     ("Monthly Balance" "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --monthly --collapse")
     ("Monthly Expence" "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --monthly --sort -amount")
     ("Yearly Balance" "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 資産 負債 資本 --yearly --collapse")
     ("Yearly Expence" "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared 支出 --yearly --sort -amount")
     ("Account Statement" "%(binary) reg -f %(ledger-file) --explicit --pedantic --cleared %(account)")) t)
 '(make-backup-files nil)
 '(markdown-fontify-code-block-natively t t)
 '(minions-direct '(flycheck-mode))
 '(minions-mode t)
 '(mozc-candidate-style 'popup)
 '(org-agenda-files
   '("~/Dropbox/org/inbox.org" "~/Dropbox/org/gtd.org" "~/Dropbox/org/tickler.org") t)
 '(org-agenda-text-search-extra-files '(agenda-archives) t)
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t)) t)
 '(org-capture-templates
   '(("t" "Task" entry
      (file "~/Dropbox/org/inbox.org")
      "* TODO %?
%U
%a" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/org/inbox.org")
      "* %?
%U
%a" :prepend t)) t)
 '(org-edit-src-content-indentation 0 t)
 '(org-log-done 'time t)
 '(org-log-into-drawer t t)
 '(org-modules '(org-docview org-habit org-info) t)
 '(org-outline-path-complete-in-steps nil t)
 '(org-refile-targets
   '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
     ("~/Dropbox/org/someday.org" :level . 1)
     ("~/Dropbox/org/tickler.org" :maxlevel . 3)) t)
 '(org-refile-use-outline-path 'file t)
 '(org-replace-disputed-keys t t)
 '(org-reverse-note-order t t)
 '(org-src-window-setup 'current-window t)
 '(org-startup-folded 'content t)
 '(org-startup-indented t t)
 '(org-tag-alist '(("@office" . 111) ("@home" . 104)) t)
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)")) t)
 '(org-use-speed-commands t t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(zeal-at-point yasnippet-snippets which-key web-mode undo-tree solarized-theme smex rg projectile pocket-reader php-mode ob-async mozc-popup minions markdown-mode magit ledger-mode leaf-keywords json-mode ivy-rich ivy-hydra helpful google-translate git-timemachine flycheck-ledger expand-region exec-path-from-shell edit-server dockerfile-mode docker-compose-mode counsel company-quickhelp))
 '(projectile-completion-system 'ivy)
 '(projectile-mode t nil (projectile))
 '(recentf-max-saved-items 1024 t)
 '(require-final-newline t)
 '(ruby-insert-encoding-magic-comment nil t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(sh-basic-offset 2 t)
 '(show-paren-mode t)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-variable-pitch nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit default))) nil "Customized with leaf in markdown-mode block"))
