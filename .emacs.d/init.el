;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

(package-initialize)

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
 '(counsel-describe-function-function (quote helpful-callable))
 '(counsel-describe-variable-function (quote helpful-variable))
 '(counsel-mode t)
 '(create-lockfiles nil)
 '(docker-tramp-use-names t)
 '(electric-pair-mode t)
 '(flycheck-global-modes t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-rich-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote abbreviate))
 '(ledger-post-amount-alignment-column 64)
 '(ledger-reports
   (quote
    (("Balance Sheet" "%(binary) -f %(ledger-file) bal --explicit --pedantic --cleared 資産 負債 資本")
     ("Monthly Balance" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly 資産 負債 資本 --collapse")
     ("Monthly Expence" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared --monthly 支出 --sort -amount")
     ("Account Statement" "%(binary) -f %(ledger-file) reg --explicit --pedantic --cleared %(account)"))))
 '(make-backup-files nil)
 '(minions-direct (quote (flycheck-mode)))
 '(minions-mode t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/inbox.org" "~/Dropbox/org/gtd.org" "~/Dropbox/org/tickler.org")))
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file "~/Dropbox/org/inbox.org")
      "* TODO %?
%U
%a" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/org/inbox.org")
      "* %?
%U
%a" :prepend t))))
 '(org-edit-src-content-indentation 0)
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-modules (quote (org-docview org-habit org-info)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets
   (quote
    (("~/Dropbox/org/gtd.org" :maxlevel . 3)
     ("~/Dropbox/org/someday.org" :level . 1)
     ("~/Dropbox/org/tickler.org" :maxlevel . 3))))
 '(org-refile-use-outline-path (quote file))
 '(org-reverse-note-order t)
 '(org-src-window-setup (quote current-window))
 '(org-tag-alist (quote (("@office" . 111) ("@home" . 104))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(@c/!)"))))
 '(org-use-speed-commands t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (solarized-theme exec-path-from-shell minions ivy counsel swiper ivy-rich smex company company-quickhelp yasnippet yasnippet-snippets flycheck expand-region undo-tree which-key helpful magit projectile docker docker-tramp dockerfile-mode docker-compose-mode markdown-mode json-mode ledger-mode flycheck-ledger)))
 '(prog-mode-hook (quote (flyspell-prog-mode hs-minor-mode)))
 '(projectile-completion-system (quote ivy))
 '(projectile-mode t nil (projectile))
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(show-paren-mode t)
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

;; theme
(load-theme 'solarized-dark t)

;; font
(set-face-attribute 'default nil :family "Ricty")
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty"))

;; key bindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'docker)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-h k") 'helpful-key)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; local.el
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (load local-file t))

(provide 'init)
;;; init.el ends here
