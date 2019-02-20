;;; init.el --- Emacs configuration

;;; Commentary:

;; Emacs configuration.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-=")         'er/expand-region)
(global-set-key (kbd "C-c a")       'org-agenda)
(global-set-key (kbd "C-c c")       'org-capture)
(global-set-key (kbd "C-c d")       'docker)
(global-set-key (kbd "C-c m")       'magit-status)
(global-set-key (kbd "C-c p")       'projectile-command-map)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-h f")       'helpful-callable)
(global-set-key (kbd "C-h k")       'helpful-key)
(global-set-key (kbd "C-h v")       'helpful-variable)
(global-set-key (kbd "C-s")         'swiper)
(global-set-key (kbd "C-x C-b")     'ibuffer)

(when window-system
  (load-theme 'solarized-dark t)
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 140)
  (set-fontset-font t 'unicode (font-spec :family "Ricty Dminished")))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(with-eval-after-load 'ledger-mode
  (add-hook 'ledger-mode-hook
            (lambda() (add-hook 'before-save-hook 'ledger-mode-clean-buffer nil t))))

;;; init.el ends here
