(setq gc-cons-threshold 20000000)

(setq inhibit-splash-screen t)
(setq-default tab-width 3)
(setq auto-save-default nil)
(set-frame-font "Source Code Pro Medium-11")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
(electric-pair-mode 1)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq python-shell-interpreter "python3.6")
(defvaralias 'flycheck-python-pylint-executable 'python-shell-interpreter)

(setq js-indent-level 2)

;;------------------------------------------------------------------------------

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;;------------------------------------------------------------------------------

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-whitespace-mode t)

;;------------------------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
(package-install 'use-package))

;-------------------------------------------------------------------------------

(use-package better-defaults
  :ensure t)


(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-eighties t))


(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))


(use-package ag
  :ensure t
  :defer t
  :commands (ag ag-regexp ag-project))


(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))


(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))


(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))


(use-package projectile
  :ensure t
  :config
  (projectile-mode))


(use-package magit
  :ensure t
  :defer 3
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))


(use-package flycheck
  :ensure t
  :defer 3
  :config (global-flycheck-mode 1))


(use-package markdown-mode
  :ensure t
  :commands
  (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown"))


(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))


(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))


(use-package coffee-mode
  :ensure t
  :mode
  ("\\.coffee\\'" . coffee-mode)
  :init
  (setq coffee-tab-width 2))


(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  :init
  (setq typescript-indent-level 2))


(use-package web-mode
  :ensure t
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)))


(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode))


(use-package vue-mode
  :ensure t
  :mode
  ("\\.vue\\'" . vue-mode))


(use-package pug-mode
  :ensure t
  :mode
  ("\\.pug\\'" . pug-mode)
  :init
  (setq pug-tab-width 2))


(use-package prettier-js
  :ensure t
  :init
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '(
                           "--no-semi"
                           "--single-quote"
                           "--trailing-comma" "es5"
                           "--arrow-parens" "always")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package meghanada
  :defer t
  :ensure t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :commands
  (meghanada-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme web-mode use-package typescript-mode smex realgud projectile php-mode molokai-theme meghanada markdown-mode magit ido-ubiquitous highlight-symbol google-c-style go-mode flx-ido expand-region dracula-theme diminish color-theme-sanityinc-tomorrow coffee-mode better-defaults base16-theme autodisass-java-bytecode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
