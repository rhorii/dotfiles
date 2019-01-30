;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
 '(css-indent-offset 2)
 '(current-language-environment "Japanese")
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(highlight-symbol-idle-delay 0.5)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-rich-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote abbreviate))
 '(js-indent-level 2)
 '(ledger-post-amount-alignment-column 62)
 '(make-backup-files nil)
 '(minions-direct (quote (flycheck-mode)))
 '(minions-mode t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (expand-region volatile-highlights highlight-symbol async company company-quickhelp counsel docker docker-compose-mode dockerfile-mode exec-path-from-shell fish-mode flycheck flycheck-ledger helpful ivy ivy-rich json-mode ledger-mode magit markdown-mode minions paradox projectile rainbow-delimiters smartparens smex solarized-theme swiper undo-tree which-key yaml-mode yasnippet yasnippet-snippets)))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(prog-mode-hook
   (quote
    (flyspell-prog-mode hs-minor-mode rainbow-delimiters-mode highlight-symbol-mode highlight-symbol-nav-mode)))
 '(projectile-completion-system (quote ivy))
 '(projectile-mode t nil (projectile))
 '(recentf-max-saved-items 512)
 '(recentf-mode t)
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(sh-basic-offset 2)
 '(show-smartparens-global-mode t)
 '(shr-use-fonts nil)
 '(smartparens-global-mode t)
 '(solarized-scale-org-headlines nil)
 '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(volatile-highlights-mode t)
 '(which-key-mode t)
 '(x-underline-at-descent-line t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; Theme
(load-theme 'solarized-dark t)

;; Font
(set-face-attribute 'default nil :family "Ricty Diminished" :height 140)
(set-fontset-font t 'unicode (font-spec :family "Ricty Dminished"))

;; Key Bindings
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c d") 'docker)
(global-set-key (kbd "C-c p") 'projectile-command-map)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; init.el ends here
