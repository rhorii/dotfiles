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

(leaf *Editing
  :custom
  ;; Editing Basics
  (require-final-newline . t)
  (tab-width . 2)
  ;; Indent
  (indent-tabs-mode . nil))

(leaf *Convenience
  :custom
  (confirm-kill-emacs . 'yes-or-no-p))

(leaf *Files
  :custom
  (create-lockfiles . nil)
  ;; Auto Save
  `(auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
  `(auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))
  ;; Backup
  `(backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                               (,tramp-file-name-regexp . nil)))
  (delete-old-versions . t)
  (version-control . t))

(leaf *Environment
  :bind
  ("C-s-f" . toggle-frame-fullscreen)
  :custom
  ;; Frames
  (frame-resize-pixelwise . t)
  (tool-bar-mode . nil)
  (scroll-bar-mode . nil)
  ;; Frames > Cursor
  (blink-cursor-mode . nil)
  ;; Minibuffer
  (enable-recursive-minibuffers . t)
  (history-delete-duplicates . t)
  (history-length . t)
  ;; Mode Line
  (column-number-mode . t)
  ;; Windows
  (scroll-conservatively . 100))

(leaf *Faces
  :config
  (cond
   ((memq window-system '(ns mac))
    (set-face-attribute 'default nil :family "HackGenNerd" :height 130)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "HackGenNerd")))
   ((memq window-system '(x pgtk))
    (set-face-attribute 'default nil :family "Ricty" :height 120))))

(leaf *Help
  :custom
  ;; Customize
  `(custom-file . ,(locate-user-emacs-file "custom.el")))

(leaf *macOS
  :when (memq window-system '(ns mac))
  :custom
  (mac-auto-ascii-mode . t)
  (mac-command-modifier . 'super)
  (mac-option-modifier . 'meta))

(leaf ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys . '(?a ?s ?d ?f ?j ?k ?l)))

(leaf autorevert
  :custom
  (auto-revert-check-vc-info . nil)
  :global-minor-mode global-auto-revert-mode)

(leaf company
  :ensure t
  :blackout t
  :custom
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-show-numbers . t)
  :global-minor-mode global-company-mode)

(leaf company-prescient
  :disabled t
  :ensure t
  :after prescient company
  :global-minor-mode t)

(leaf company-statistics
  :ensure t
  :after company
  :global-minor-mode t)

(leaf company-quickhelp
  :ensure t
  :after company
  :global-minor-mode t)

(leaf compile
  :custom
  (compilation-scroll-output . 'first-error))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf docker
  :ensure t
  :bind
  ("C-c d" . docker))

(leaf docker-compose-mode :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf dumb-jump
  :ensure t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-selector . 'completing-read))

(leaf eldoc
  :custom
  (eldoc-minor-mode-string . nil))

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf exec-path-from-shell
  :when window-system
  :ensure t
  :config
  (setenv "LANG" "ja_JP.UTF-8")
  (exec-path-from-shell-initialize))

(leaf expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(leaf flycheck
  :ensure t
  :global-minor-mode global-flycheck-mode)

(leaf flycheck-color-mode-line
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode-hook . flycheck-color-mode-line-mode))

(leaf flycheck-ledger
  :ensure t
  :after flycheck
  :require t)

(leaf flyspell
  :blackout t
  :hook
  (text-mode-hook)
  (prog-mode-hook . flyspell-prog-mode))

(leaf gcmh
  :ensure t
  :blackout t
  :global-minor-mode t)

(leaf git-link
  :ensure t
  :defvar git-link-remote-alist git-link-commit-remote-alist
  :bind
  ("C-c g l" . git-link)
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

(leaf git-timemachine :ensure t)

(leaf google-this
  :ensure t
  :bind
  ("C-c s g" . google-this))

(leaf google-translate
  :ensure t
  :preface
  (defun ad:google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :advice
  (:override google-translate--search-tkk ad:google-translate--search-tkk)
  :custom
  (google-translate-backend-method . 'curl)
  (google-translate-pop-up-buffer-set-focus . t)
  (google-translate-translation-directions-alist . '(("en" . "ja")
                                                     ("ja" . "en"))))

(leaf helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(leaf hideshow
  :blackout hs-minor-mode
  :hook
  (prog-mode-hook . hs-minor-mode))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf json-mode :ensure t)

(leaf ledger-mode
  :ensure t
  :custom
  (ledger-post-amount-alignment-column . 65)
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

(leaf magit
  :ensure t
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf git-gutter-fringe
  :ensure t
  :require t
  :global-minor-mode global-git-gutter-mode)

(leaf markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-block-natively . t)
  :custom-face
  (markdown-code-face . '((t (:inherit default)))))

(leaf midnight
  :global-minor-mode t)

(leaf minions
  :disabled t
  :ensure t
  :custom
  (minions-direct . '(flycheck-mode))
  :global-minor-mode t)

(leaf mozc
  :when (memq window-system '(x pgtk))
  :ensure t
  :custom
  (default-input-method . "japanese-mozc"))

(leaf mozc-popup
  :when (memq window-system '(x pgtk))
  :ensure t
  :after mozc
  :require t
  :custom
  (mozc-candidate-style . 'popup))

(leaf mwim
  :ensure t
  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(leaf org
  :ensure t
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :custom
  (org-default-notes-file . "~/org/notes.org")
  (org-replace-disputed-keys . t)
  (org-startup-folded . 'content)
  (org-startup-indented . t))

(leaf org
  :disabled t
  :ensure t
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
    :ensure t
    :require t))

(leaf paradox
  :disabled t
  :ensure t
  :custom
  (paradox-execute-asynchronously . t)
  (paradox-github-token . t))

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf prescient
  :disabled t
  :ensure t
  :custom
  (prescient-aggressive-file-save . t)
  (prescient-sort-length-enable . nil)
  :global-minor-mode prescient-persist-mode)

(leaf php-mode :ensure t)

(leaf pocket-reader :ensure t)

(leaf projectile
  :ensure t
  :blackout t
  :bind
  (:projectile-mode-map
   ("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching . t)
  (projectile-project-search-path . '("~/src/"))
  :global-minor-mode t)

(leaf recentf
  :custom
  (recentf-max-saved-items . nil)
  :global-minor-mode t)

(leaf rg
  :ensure t
  :bind
  ("C-c s r" . rg-menu))

(leaf ruby-mode
  :custom
  (ruby-insert-encoding-magic-comment . nil))

(leaf savehist
  :custom
  (savehist-additional-variables . '(projectile-project-command-history))
  :global-minor-mode t)

(leaf sh-script
  :custom
  (sh-basic-offset . 2))

(leaf shackle
  :ensure t
  :custom
  (shackle-rules . '((vterm-mode :align 'below :size 0.2)))
  :global-minor-mode t)

(leaf so-long
  :global-minor-mode global-so-long-mode)

(leaf solarized-theme
  :when window-system
  :ensure t
  :custom
  (solarized-scale-org-headlines . nil)
  (solarized-use-variable-pitch . nil)
  (x-underline-at-descent-line . t)
  :config
  (load-theme 'solarized-dark t))

(leaf undo-tree
  :ensure t
  :blackout t
  :global-minor-mode global-undo-tree-mode)

(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'forward))

(leaf vterm
  :ensure t
  :custom
  (vterm-max-scrollback . 10000))

(leaf vterm-toggle
  :ensure t
  :bind
  ("C-`" . vterm-toggle)
  :custom
  (vterm-toggle-scope . 'project))

(leaf web-mode
  :ensure t
  :mode "\\.x[ms]l\\'"
  :custom
  (web-mode-script-padding . 4))

(leaf which-key
  :ensure t
  :blackout t
  :global-minor-mode t)

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf winner-mode
  :global-minor-mode t)

(leaf xref
  :ensure t
  :custom
  (xref-show-definitions-function . 'xref-show-definitions-completing-read))

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :hook (prog-mode-hook . yas-minor-mode))

(leaf yasnippet-snippets
  :ensure t)

(leaf dash-at-point
  :when (memq window-system '(ns mac))
  :ensure t
  :bind ([remap apropos-documentation] . dash-at-point-with-docset))

(leaf zeal-at-point
  :when (memq window-system '(x pgtk))
  :ensure t
  :bind ([remap apropos-documentation] . zeal-at-point))

(leaf migemo
  :when (executable-find "cmigemo")
  :ensure t
  :hook
  (after-init-hook . migemo-init)
  :custom
  (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))

(leaf orderless
  :ensure t
  :require t
  :custom
  (completion-styles . '(orderless))
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
  :ensure t
  :custom
  (vertico-count . 20)
  (vertico-cycle . t)
  :global-minor-mode t)

(leaf consult
  :ensure t
  :defun project-roots consult-customize
  :defvar consult-ripgrep consult-git-grep consult-grep
          consult-bookmark consult-recent-file consult-xref
          consult--source-file consult--source-project-file consult--source-bookmark
  :bind
  ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
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
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  :custom
  (consult-project-root-function . (lambda ()
                                     (when-let (project (project-current))
                                       (car (project-roots project)))))
  :init
  (leaf consult-flycheck :ensure t)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-.")))

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf embark
  :ensure t
  :bind
  ("C-h B" . embark-bindings)
  :bind*
  ("C-." . embark-act)
  :custom
  (embark-verbose-indicator-display-action . '(display-buffer-at-bottom (window-height . fit-window-to-buffer))))

(leaf embark-consult
  :ensure t
  :after embark consult
  :require t)

(provide 'init)
;;; init.el ends here
(put 'set-goal-column 'disabled nil)
