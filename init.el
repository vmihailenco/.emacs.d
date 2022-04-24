(setq gc-cons-threshold 100000000)

(setq inhibit-splash-screen t)
(setq-default tab-width 3)
(set-frame-font "Source Code Pro Medium-11")

;; don't create lock files
(setq create-lockfiles nil)
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(electric-pair-mode 1)

(setq python-shell-interpreter "python3.8")
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

(setq-default whitespace-style '(face trailing lines empty))
(setq-default whitespace-line-column 100)
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

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;-------------------------------------------------------------------------------

(use-package flyspell
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  :config
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq ispell-local-dictionary-alist
        `((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))
  )

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;-------------------------------------------------------------------------------

(use-package ivy
  :defer t
  :diminish
  :config
  (ivy-mode))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy-rich
  :hook (counsel-mode . ivy-rich-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;-------------------------------------------------------------------------------

(use-package better-defaults)


(use-package which-key
  :diminish
  :config
  (which-key-mode))


 (use-package base16-theme
   :config
   (load-theme 'base16-eighties t))


(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))


(use-package highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))


(use-package ag
  :defer t
  :commands (ag ag-regexp ag-project))


(use-package projectile
  :diminish
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package magit
  :defer 3
  :bind
  ("C-x g" . magit-status))


(use-package flycheck
  :defer 3
  :config
  (global-flycheck-mode 1))


(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown"))


(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))


(use-package kotlin-mode
  :mode ("\\.kts\\'" . kotlin-mode))


(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))


(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "gofumports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))


(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :init)


(use-package coffee-mode
  :mode
  ("\\.coffee\\'" . coffee-mode)
  :init
  (setq coffee-tab-width 2))


(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  :init
  (setq typescript-indent-level 2))


(use-package web-mode
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.html\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  ("\\.mjml\\'" . web-mode)
  :custom
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  (web-mode-block-padding 0)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode))

(use-package php-mode
  :mode
  ("\\.php\\'" . php-mode))


(use-package css-mode
  :init
  (setq css-indent-offset 2))


(use-package rubocopfmt
  :hook
  (ruby-mode . rubocopfmt-mode)
  :custom
  (rubocopfmt-use-bundler-when-possible nil))

(use-package edit-indirect)


(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))


(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))


;; (use-package vue-mode
;;   :ensure t
;;   :mode
;;   ("\\.vue\\'" . vue-mode))


;; (use-package mmm-mode
;;   :ensure t
;;   :init
;;   (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;;   (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))


(use-package pug-mode
  :mode
  ("\\.pug\\'" . pug-mode)
  :init
  (setq pug-tab-width 2))


(use-package scss-mode
  :mode
  ("\\.scss\\'" . scss-mode))


(use-package prettier-js
  :config
  (add-hook 'xml-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'java-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'scss-mode-hook 'prettier-js-mode)
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (add-hook 'yaml-mode-hook 'prettier-js-mode))


(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package langtool
  :init
  (setq langtool-language-tool-jar "/home/vmihailenco/Downloads/LanguageTool-5.1/languagetool-commandline.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-enable-completion-at-point nil)
;;   :commands
;;   (lsp lsp-deferred)
;;   :hook
;;   (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;(defun lsp-go-install-save-hooks ()
;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
;(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; ;; Optional - provides fancier overlays.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; ;; Company mode is a standard completion package that works well with lsp-mode.
;; (use-package company
;;   :ensure t
;;   :config
;;   ;; Optionally enable completion-as-you-type behavior.
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1))

;; ;; company-lsp integrates company mode completion with lsp-mode.
;; ;; completion-at-point also works out of the box but doesn't support snippets.
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra)

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
