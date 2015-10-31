(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(
  better-defaults
  zenburn-theme

  go-mode
  coffee-mode
  flymake-coffee
  sass-mode
  scss-mode
  less-css-mode

  magit
  full-ack
  expand-region
  autopair
  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;-------------------------------------------------------------------------------

(require 'better-defaults)

(setq inhibit-splash-screen t)
(setq tab-width 4)
(setq default-tab-width 4)
(setq auto-save-default nil)
(set-default-font "Consolas-11")

(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(global-whitespace-mode t)

;; disable auto wrapping
(add-hook 'html-mode-hook '(lambda () (auto-fill-mode 0)))
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;-------------------------------------------------------------------------------

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

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;------------------------------------------------------------------------------

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;------------------------------------------------------------------------------

(require 'autopair)
(autopair-global-mode)

;;------------------------------------------------------------------------------

(require 'go-mode)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-to-list 'load-path "~/workspace/gocode/src/github.com/dougm/goflymake")
(require 'go-flymake)

;;----------------------------------------------------------------------------=

(require 'haml-mode)
(require 'sass-mode)

;;------------------------------------------------------------------------------

(setq scss-compile-at-save nil)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode
          (set (make-local-variable 'tab-width) 2))

;;------------------------------------------------------------------------------

(require 'coffee-mode)

; set tab-width = 2 special for coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;------------------------------------------------------------------------------

(load-theme 'zenburn t)

;;------------------------------------------------------------------------------
