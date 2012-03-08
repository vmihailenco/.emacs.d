(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;===============================================================================

(setq tab-width 4)
(setq default-tab-width 4)
(setq auto-save-default nil)
(set-default-font "Consolas-11")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(global-whitespace-mode t)

;; disable auto wrapping
(add-hook 'html-mode-hook '(lambda () (auto-fill-mode 0)))

;===============================================================================

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

;===============================================================================

(add-to-list 'load-path "~/.emacs.d/vendor/projutils.el.d")
(require 'projutils)
(projutils-global-mode 1)
(setq projutils-grep-use-ack 1)
(setq-default projutils-ffip-allowed-file-extensions
              '("txt" "py" "html" "js" "css" "coffee" "sass"))

;===============================================================================

(require 'autopair)
(autopair-global-mode)

;===============================================================================

(add-to-list 'load-path "~/workspace/go/misc/emacs")
(require 'go-mode-load)

(add-hook 'before-save-hook #'gofmt-before-save)

;===============================================================================

(add-to-list 'load-path "~/.emacs.d/vendor/Pymacs")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;-------------------------------------------------------------------------------

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;-------------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/vendor/python.el.d")
(require 'python)

(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-index)))

(add-hook 'python-mode-hook 'esk-local-column-number-mode)
(add-hook 'python-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'python-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'python-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'python-mode-hook 'esk-pretty-lambdas)
(add-hook 'python-mode-hook 'esk-add-watchwords)
(add-hook 'python-mode-hook 'esk-turn-on-idle-highlight-mode)

;-------------------------------------------------------------------------------

; Flymake is implemented as an Emacs minor mode. It runs the syntax check tool
; (the compiler for C++ files, perl for perl files, etc.) in the background,
; passing it a temporary copy of the current buffer and parses the output for
; known error/warning message patterns. Flymake then highlights erroneous lines
; (that is, lines for which at least one error or warning has been reported),
; and displays an overall buffer status in the mode line, as shown on the figure
; below.

(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/checkers" (list "--checkers=pep8,pyflakes" local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycheckers-init)))

(add-hook 'python-mode-hook
          (lambda ()
            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key [f2] 'flymake-goto-prev-error)
            (local-set-key [f3] 'flymake-goto-next-error)))

;==============================================================================

(when (load "flymake" t)
  (defun flymake-csscheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/checkers" (list "--checkers=csslint" local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.css\\'" flymake-csscheckers-init)))

(add-hook 'css-mode-hook
          (lambda ()
            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key [f2] 'flymake-goto-prev-error)
            (local-set-key [f3] 'flymake-goto-next-error)))

;===============================================================================

(require 'haml-mode)
(require 'sass-mode)

;-------------------------------------------------------------------------------

(setq scss-compile-at-save nil)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode
          (set (make-local-variable 'tab-width) 2))

;===============================================================================

(require 'coffee-mode)

; set tab-width = 2 special for coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;===============================================================================

(require 'yasnippet)
(yas/initialize)

;===============================================================================

(require 'color-theme-zenburn)
(color-theme-zenburn)

;===============================================================================

; http://nschum.de/src/emacs/full-ack/

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;===============================================================================

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(
                      starter-kit
                      starter-kit-lisp
                      starter-kit-js
                      starter-kit-bindings
                      starter-kit-eshell
                      color-theme-zenburn
                      coffee-mode
                      flymake-coffee
                      sass-mode
                      scss-mode
                      yasnippet
                      full-ack
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
