(setq gc-cons-threshold 20000000)

(setq inhibit-splash-screen t)
(setq-default tab-width 3)
(setq auto-save-default nil)
(set-frame-font "Source Code Pro Medium-11")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
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

(defun my-delete-trailing-whitespace ()
  (when (not (string= (file-name-extension buffer-file-name) "http"))
    (delete-trailing-whitespace)))

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
(global-whitespace-mode t)

;;------------------------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
(package-install 'use-package))

;-------------------------------------------------------------------------------

(use-package better-defaults
  :ensure t)


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-eighties t))


(use-package smartparens
  :ensure t
  :init
  (use-package smartparens-config)
  (use-package smartparens-python)
  (use-package smartparens-html)
  (use-package smartparens-markdown)
  (use-package smartparens-javascript)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :config
  (setq smartparens-strict-mode t))


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
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


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


(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))


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


(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup)
  :init)


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


(use-package mmm-mode
  :ensure t
  :init
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))


(use-package vue-html-mode
  :ensure t)


(use-package css-mode
  :init
  (setq css-indent-offset 2))


(use-package edit-indirect
  :ensure t)


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


(use-package scss-mode
  :ensure t
  :mode
  ("\\.scss\\'" . scss-mode))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'scss-mode-hook 'prettier-js-mode)
  (add-hook 'xml-hook 'prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all")))

(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t)

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z l") 'hydra-lsp/body)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
