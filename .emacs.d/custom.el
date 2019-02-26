(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
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
 '(electric-pair-mode t)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-pycompile-executable "python3")
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote abbreviate))
 '(ledger-post-amount-alignment-column 62)
 '(make-backup-files nil)
 '(minions-direct (quote (flycheck-mode)))
 '(minions-mode t)
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("g" . "GTD")
     ("gi" "inbox" tags "inbox" nil)
     ("gw" "waiting for" todo "WAITING" nil)
     ("gs" "someday/maybe" todo "SOMEDAY" nil))))
 '(org-agenda-files (quote ("~/org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file "~/org/inbox.org")
      "* TODO %?
  %U")
     ("n" "Note" entry
      (file "~/org/inbox.org")
      "* %?
  %U"))))
 '(org-default-notes-file "~/org/inbox.org")
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-refile-use-outline-path (quote file))
 '(org-tag-alist
   (quote
    ((:startgroup)
     ("@home" . 104)
     ("@office" . 111)
     ("@errands" . 101)
     (:endgroup)
     ("project" . 112)
     ("note" . 110))))
 '(org-tags-exclude-from-inheritance (quote ("project")))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELED(c@/!)"))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (async company company-quickhelp counsel docker docker-compose-mode dockerfile-mode edit-indirect exec-path-from-shell expand-region fish-mode flycheck flycheck-ledger helpful ivy json-mode ledger-mode magit markdown-mode minions paradox projectile smex solarized-theme swiper undo-tree which-key yaml-mode yasnippet yasnippet-snippets)))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(prog-mode-hook (quote (flyspell-prog-mode hs-minor-mode)))
 '(projectile-completion-system (quote ivy))
 '(projectile-mode t nil (projectile))
 '(recentf-max-saved-items 512)
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
 '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(which-key-mode t)
 '(x-underline-at-descent-line t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
