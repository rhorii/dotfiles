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
    :package t
    :config
    (leaf-keywords-init)))

(leaf no-littering
  :package t
  :require t
  :custom `(custom-file . ,(expand-file-name "custom.el" user-emacs-directory)))

(leaf files
  :custom ((version-control . t)
           (delete-old-versions . t)
           (require-final-newline . t)
           (confirm-kill-emacs . 'yes-or-no-p)))

(leaf simple
  :custom (column-number-mode . t))

(leaf faces
  :config
  (cond
   ((memq window-system '(ns mac))
    (set-face-attribute 'default nil :family "HackGenNerd" :height 130)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "HackGenNerd")))
   ((memq window-system '(x pgtk))
    (set-face-attribute 'default nil :family "Ricty" :height 120))))

(leaf frame
  :bind ("C-s-f" . toggle-frame-fullscreen)
  :custom (blink-cursor-mode . nil))

(leaf scroll-bar
  :custom (scroll-bar-mode . nil))

(leaf cus-start
  :custom ((tab-width . 2)
           (delete-by-moving-to-trash . t)
           (create-lockfiles . nil)
           (tool-bar-mode . nil)
           (frame-resize-pixelwise . t)
           (indent-tabs-mode . nil)
           (enable-recursive-minibuffers . t)
           (history-length . t)
           (history-delete-duplicates . t)
           (scroll-conservatively . 100)))

(leaf mac-win
  :when (memq window-system '(ns mac))
  :custom ((mac-auto-ascii-mode . t)
           (mac-command-modifier . 'super)
           (mac-option-modifier . 'meta)))

(leaf exec-path-from-shell
  :when window-system
  :package t
  :config
  (setenv "LANG" "ja_JP.UTF-8")
  (exec-path-from-shell-initialize))

(leaf gcmh
  :package t
  :global-minor-mode t)

(leaf midnight
  :global-minor-mode t)

(leaf so-long
  :global-minor-mode global-so-long-mode)

(leaf helpful
  :package t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(leaf which-key
  :package t
  :global-minor-mode t)

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf winner-mode
  :global-minor-mode t)

(leaf ace-window
  :package t
  :bind ([remap other-window] . ace-window)
  :custom (aw-keys . '(?a ?s ?d ?f ?j ?k ?l)))

(leaf autorevert
  :custom (auto-revert-check-vc-info . nil)
  :global-minor-mode global-auto-revert-mode)

(leaf recentf
  :custom (recentf-max-saved-items . nil)
  :global-minor-mode t)

(leaf savehist
  :custom (savehist-additional-variables . '(projectile-project-command-history))
  :global-minor-mode t)

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf hideshow
  :hook (prog-mode-hook . hs-minor-mode))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf uniquify
  :custom (uniquify-buffer-name-style . 'post-forward-angle-brackets))

(leaf volatile-highlights
  :package t
  :global-minor-mode t)

(leaf corfu
  :package t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-cycle . t)
  (corfu-quit-at-boundary . nil))

(leaf corfu-doc
  :package t
  :after corfu
  :bind (:corfu-map
         ("M-p" . corfu-doc-scroll-down)
         ("M-n" . corfu-doc-scroll-up))
  :hook (corfu-mode-hook))

(leaf kind-icon
  :package t
  :after corfu
  :require t
  :defun kind-icon-margin-formatter
  :defvar corfu-margin-formatters
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf cape
  :package t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(leaf yasnippet
  :package t
  :hook (prog-mode-hook . yas-minor-mode)
  :init
  (leaf yasnippet-snippets :package t))

(leaf vundo
  :package t
  :bind ([remap undo] . vundo)
  :custom
  (vundo-roll-back-on-quit . nil))

(leaf mozc
  :when (memq window-system '(x pgtk))
  :package t
  :custom (default-input-method . "japanese-mozc")
  :defer-config
  (leaf mozc-popup
    :package t
    :require t
    :custom (mozc-candidate-style . 'popup)))

(leaf mwim
  :package t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(leaf subword
  :global-minor-mode global-subword-mode)

(leaf expand-region
  :package t
  :bind ("C-=" . er/expand-region))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf compile
  :custom (compilation-scroll-output . 'first-error))

(leaf docker
  :package t
  :bind ("C-c d" . docker))

(leaf flycheck
  :package t
  :global-minor-mode global-flycheck-mode
  :defer-config
  (leaf flycheck-color-mode-line
    :package t
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode)))

(leaf flyspell
  :hook ((text-mode-hook)
         (prog-mode-hook . flyspell-prog-mode)))

(leaf minions
  :package t
  :custom (minions-direct . '(flycheck-mode))
  :global-minor-mode t)

(leaf shackle
  :package t
  :custom (shackle-rules . '((vterm-mode :align 'below :size 0.2)))
  :global-minor-mode t)

(leaf migemo
  :when (executable-find "cmigemo")
  :package t
  :hook (after-init-hook . migemo-init)
  :custom (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))

(leaf orderless
  :package t
  :require t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf *orderless-migemo
  :after orderless migemo
  :defun migemo-get-pattern orderless-define-completion-style orderless-matching-styles
  :defvar orderless-migemo-style
  :config
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (orderless-define-completion-style orderless-migemo-style
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp
                                 orderless-migemo)))
  (add-to-list 'completion-category-overrides '(consult-location (styles orderless-migemo-style))))

(leaf vertico
  :package t
  :custom ((vertico-count . 20)
           (vertico-cycle . t))
  :global-minor-mode t)

(leaf consult
  :package t
  :defun project-roots consult-customize
  :defvar consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
  :bind
  ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c k" . consult-kmacro)
  ("C-c b" . consult-bookmark)
  ;; C-x bindings (ctl-x-map)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ;; Other custom bindings
  ([remap yank-pop] . consult-yank-pop)
  ([remap apropos-command] . consult-apropos)
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flycheck)
  ([remap goto-line] . consult-goto-line)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings (search-map)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ([remap isearch-forward] . consult-line)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode)
  :custom
  (consult-project-root-function . (lambda ()
                                     (when-let (project (project-current))
                                       (car (project-roots project)))))
  (xref-show-xrefs-function . 'consult-xref)
  (xref-show-definitions-function . 'consult-xref)
  :init
  (leaf consult-flycheck :package t)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

(leaf marginalia
  :package t
  :custom (marginalia-field-width . 120)
  :global-minor-mode t)

(leaf embark
  :package t
  :bind
  ("C-h B" . embark-bindings)
  :bind*
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  :custom
  (embark-verbose-indicator-display-action . '(display-buffer-at-bottom (window-height . fit-window-to-buffer)))
  (prefix-help-command . 'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :package t
  :after embark consult
  :require t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf solarized-theme
  :when window-system
  :package t
  :custom ((solarized-scale-org-headlines . nil)
           (solarized-use-variable-pitch . nil)
           (x-underline-at-descent-line . t))
  :config
  (load-theme 'solarized-dark t))

(leaf vterm
  :package t
  :custom ((vterm-max-scrollback . 10000)
           (vterm-buffer-name-string . "vterm: %s"))
  :config
  (leaf vterm-toggle
    :package t
    :bind (("C-`" . vterm-toggle)
           ("s-{" . vterm-toggle-backward)
           ("s-}" . vterm-toggle-forward))
    :custom (vterm-toggle-scope . 'project)))

(leaf projectile
  :package t
  :bind (:projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :custom ((projectile-project-search-path . '("~/src/"))
           (projectile-switch-project-action . 'projectile-vc))
  :global-minor-mode t)

(leaf perspective
  :disabled t
  :package t
  :bind (([remap switch-to-buffer] . persp-switch-to-buffer*)
         ([remap kill-buffer] . persp-kill-buffer*))
  :global-minor-mode persp-mode)

(leaf magit
  :package t
  :custom (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf git-gutter-fringe
  :package t
  :require t
  :global-minor-mode global-git-gutter-mode)

(leaf git-link
  :package t
  :defvar git-link-remote-alist git-link-commit-remote-alist
  :bind ("C-c L" . git-link)
  :config
  (defun git-link-backlog (hostname dirname filename branch commit start end)
    (format "https://%s.backlog.jp/git/%s/blob/%s/%s"
            (car (split-string hostname "\\."))
	          dirname
	          (or branch commit)
	          (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "%s-%s" start end)
                                (format "%s" start)))))))
  (defun git-link-commit-backlog (hostname dirname commit)
    (format "https://%s.backlog.jp/git/%s/commit/%s"
	          (car (split-string hostname "\\."))
	          dirname
	          commit))
  (add-to-list 'git-link-remote-alist '("backlog" git-link-backlog))
  (add-to-list 'git-link-commit-remote-alist '("backlog" git-link-commit-backlog)))

(leaf git-timemachine :package t)

(leaf lsp-mode
  :package t
  :preface
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode-hook . lsp-mode-setup-completion)
  (php-mode-hook . lsp-deferred)
  :custom
  (lsp-completion-provider . :none))

(leaf lsp-ui
  :disabled t
  :package t
  :after lsp-mode)

(leaf rg :package t)

(leaf xref :package t)

(leaf dumb-jump
  :package t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom (dumb-jump-selector . 'completing-read))

(leaf google-this
  :package t
  :bind ("C-c s" . google-this))

(leaf google-translate
  :package t
  :preface
  (defun ad:google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :bind ("C-c t" . google-translate-smooth-translate)
  :advice (:override google-translate--search-tkk ad:google-translate--search-tkk)
  :custom ((google-translate-translation-directions-alist . '(("en" . "ja")
                                                              ("ja" . "en")))
           (google-translate-pop-up-buffer-set-focus . t)))

(leaf dash-at-point
  :when (memq window-system '(ns mac))
  :package t
  :bind ([remap apropos-documentation] . dash-at-point-with-docset))

(leaf zeal-at-point
  :when (memq window-system '(x pgtk))
  :package t
  :bind ([remap apropos-documentation] . zeal-at-point))

(leaf docker-compose-mode :package t)

(leaf dockerfile-mode :package t)

(leaf json-mode :package t)

(leaf ledger-mode
  :package t
  :custom ((ledger-post-amount-alignment-column . 65)
           (ledger-reports . '(("Balance Sheet"
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
  :defer-config
  (leaf flycheck-ledger
    :package t
    :after flycheck
    :require t))

(leaf markdown-mode
  :package t
  :custom (markdown-fontify-code-block-natively . t)
  :custom-face (markdown-code-face . '((t (:inherit default)))))

(leaf org
  :package t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom `((org-adapt-indentation . nil)
            ;; org-agenda
            (org-agenda-files . '("~/org/inbox.org" "~/org/notes.org"))
            (org-agenda-custom-commands . '((" " "Agenda"
                                             ((agenda ""
                                                      ((org-deadline-warning-days 365)))
                                              (todo "NEXT"
                                                    ((org-agenda-overriding-header "Tasks")))
                                              (tags "inbox"
                                                    ((org-agenda-overriding-header "Inbox")
                                                     (org-agenda-prefix-format "  %?-12t% s")))))))
            (org-agenda-restore-windows-after-quit . t)
            (org-agenda-format-date . "%F %a")
            (org-agenda-breadcrumbs-separator . "/")
            (org-capture-templates . '(("t" "Task" entry
                                        (file "inbox.org")
		                                    ,(concat "* TODO %?\n"
                                                 ":PROPERTIES:\n"
                                                 ":CREATED: %U\n"
                                                 ":END:"))
                                       ("n" "Note" entry
                                        (file "inbox.org")
                                        ,(concat "* %? :note:\n"
                                                 ":PROPERTIES:\n"
                                                 ":CREATED: %U\n"
                                                 ":END:"))
                                       ("m" "Meeting" entry
                                        (file "inbox.org")
                                        ,(concat "* %? :meeting:\n"
                                                 "<%<%Y-%m-%d %a %H:00>>"))
                                       ("j" "Journal" entry
                                        (file+olp+datetree "notes.org" "Journal")
                                        "* %?"
                                        :tree-type week)))
            (org-clock-out-remove-zero-time-clocks . t)
            (org-default-notes-file . "~/org/notes.org")
            (org-log-done . 'time)
            (org-log-into-drawer . t)
            (org-outline-path-complete-in-steps . nil)
            (org-refile-use-outline-path . 'file)
            (org-refile-targets . '(("notes.org" :maxlevel . 2)))
            (org-replace-disputed-keys . t)
            (org-use-speed-commands . t)
            (org-todo-keywords . '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                                   (sequence "WAIT(w@/!)" "|" "CANCELED(@c/!)")))))

(leaf php-mode :package t)

(leaf ruby-mode
  :custom (ruby-insert-encoding-magic-comment . nil))

(leaf sh-script
  :custom (sh-basic-offset . 2))

(leaf web-mode
  :package t
  :mode "\\.x[ms]l\\'" "\\.html?\\'"
  :custom (web-mode-script-padding . 4))

(provide 'init)
;;; init.el ends here
