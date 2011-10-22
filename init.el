(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;===============================================================================

(set-default-font "Consolas-11")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(global-whitespace-mode t)

(add-hook 'html-mode-hook '(lambda () (auto-fill-mode 0)))

;===============================================================================

; FFAP mode replaces certain key bindings for finding files, including C-x C-f,
; with commands that provide more sensitive defaults. These commands behave
; like the ordinary ones when given a prefix argument. Otherwise, they get the
; default file name or URL from the text around point. If what is found in the
; buffer has the form of a URL rather than a file name, the commands use
; browse-url to view it.

(ffap-bindings)

;===============================================================================

; Ido is part of Emacs, starting with release 22.
;
; The ido.el package by KimStorm lets you interactively do things with buffers
; and files.

(require 'ido)
(setq uniquify-buffer-name-style 'post-forward)

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

;(add-to-list 'load-path "~/go/misc/emacs")
;(require 'go-mode-load)

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

(add-to-list 'load-path "~/.emacs.d/vendor/haml-mode")
(require 'haml-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/sass-mode")
(require 'sass-mode)

;-------------------------------------------------------------------------------

(setq scss-compile-at-save nil)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/scss-mode/"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode
          (set (make-local-variable 'tab-width) 2))

;===============================================================================

(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)

; set tab-width = 2 special for coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;===============================================================================

(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet-0.6.1c/snippets")

;===============================================================================

;(add-to-list 'load-path "~/.emacs.d/vendor/django-mode")
;(require 'django-html-mode)
;(require 'django-mode)
;(yas/load-directory "~/.emacs.d/vendor/django-mode/snippets")
;(add-to-list 'auto-mode-alist '("\.html$" . django-html-mode))

;===============================================================================

(add-to-list 'load-path "~/.emacs.d/vendor/zenburn-emacs")
(require 'color-theme-zenburn)
(color-theme-zenburn)

;===============================================================================

(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq ".pyc" eol))))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)

;===============================================================================

; http://nschum.de/src/emacs/full-ack/

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
