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
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(counsel-mode t)
 '(create-lockfiles nil)
 '(default-input-method "japanese-mozc")
 '(docker-tramp-use-names t)
 '(edit-server-new-frame nil)
 '(electric-pair-mode t)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-pycompile-executable "python3")
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote abbreviate))
 '(ledger-post-amount-alignment-column 62)
 '(ledger-reports
   (quote
    (("Balance Sheet" "%(binary) -f %(ledger-file) bal --explicit --pedantic --cleared 資産 負債 資本")
     ("Monthly Balance" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly 資産 負債 資本 --collapse")
     ("Monthly Expence" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly 支出 --sort -amount")
     ("Account Statement" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared %(account)"))))
 '(load-prefer-newer t)
 '(make-backup-files nil)
 '(minions-direct (quote (flycheck-mode)))
 '(minions-mode t)
 '(org-agenda-files (quote ("~/Dropbox/org")))
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file "~/Dropbox/org/inbox.org")
      "* TODO %?
%U")
     ("n" "Note" entry
      (file "~/Dropbox/org/inbox.org")
      "* %?
%U"))))
 '(org-default-notes-file "~/Dropbox/org/inbox.org")
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-modules (quote (org-habit org-depend)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-refile-use-outline-path (quote file))
 '(org-replace-disputed-keys t)
 '(org-src-window-setup (quote current-window))
 '(org-tag-alist (quote (("@office" . 111) ("@home" . 104))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)"))))
 '(org-use-speed-commands t)
 '(package-selected-packages
   (quote
    (shackle edit-server yasnippet-snippets which-key use-package undo-tree solarized-theme smex projectile paradox org-plus-contrib minions markdown-mode magit ledger-mode ivy-hydra helpful flycheck-ledger fish-mode expand-region exec-path-from-shell dockerfile-mode docker-compose-mode docker csv-mode counsel company-quickhelp)))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(prog-mode-hook (quote (flyspell-prog-mode hs-minor-mode)))
 '(projectile-completion-system (quote ivy))
 '(recentf-max-saved-items 1024)
 '(recentf-mode t)
 '(require-final-newline t)
 '(ruby-insert-encoding-magic-comment nil)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
 '(shr-use-fonts nil)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-variable-pitch nil)
 '(tab-width 2)
 '(text-mode-hook (quote (turn-on-flyspell)))
 '(tool-bar-mode nil)
 '(winner-mode t)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
