; init.el
; Author: Jake Voytko
; Time-stamp: <2016-07-22 14:31:51 jvoytko>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What system are we on?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install packages if bootstrapping.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(mapc
 (lambda (package)
   (if (not (package-installed-p package))
       (package-install package)))
 '(
   ac-js2
   ac-php
   ac-php-core
   auto-complete
   company
   company-go
   company-php
   dash
   f
   find-file-in-project
   go-eldoc
   go-mode
   ivy
   js2-mode
   js2-refactor
   markdown-mode
   multiple-cursors
   mustache-mode
   php-mode
   popup
   s
   scala-mode
   scss-mode
   simple-httpd
   skewer-mode
   tern
   xcscope
   yasnippet))

;; Allow emacsclient to connect.
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load machine-specific customization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/machine.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deviation from default behaviors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Command is better positioned for meta on osx keyboards.
(if (system-is-mac)
    (setq ns-command-modifier 'meta))

;; Set the environment correctly on Mac.
(when (system-is-mac)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;; This is almost always a mistake.
(global-unset-key (kbd "C-x C-c"))

;; Enable Time-stamp functions.
(add-hook 'write-file-hooks 'time-stamp)

;; Stuff involving columns.
(setq-default fill-column 80)
(column-number-mode t)

;; Tabbing behavior (prefer spaces, indent 4). Highlight bad whitespace.
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

;; Don't mess with final newlines in files, to avoid unnecessary diffs.
(setq-default require-final-newline nil)
(setq-default mode-require-final-newline nil)
(setq-default next-line-add-newlines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'manoj-dark)

;; Initial sizing and positioning. This is good enough across a bunch of systems.
(add-to-list 'initial-frame-alist '(left . 82))
(add-to-list 'initial-frame-alist '(top . 23))
(add-to-list 'initial-frame-alist '(width . 172))
(add-to-list 'initial-frame-alist '(height . 55))

;; Create two vertical windows on startup.
(unless (> (length (window-list)) 1)
  (split-window-right))

;; Hide UI components.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Hide default splash screen.
(setq-default inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and mode customization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Markdown mode.
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; JS2 mode.
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

;; Refactoring support for js2 mode.
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; tern.js
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; ;; Go
;; (require 'go-mode)

;; ;; go-eldoc
;; (require 'go-eldoc)

;; Company mode
(require 'company)
;; (require 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease autocomplete delay
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; autocomplete only after typing

;; PHP mode.
(require 'cl)
(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             ;; ac-php
             (auto-complete-mode t)
             (require 'ac-php)
             (setq ac-sources  '(ac-source-php ) )
             (yas-global-mode 1)
             (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)
             (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)

             ;; company mode with ac-php backend
             (require 'company-php)
             (company-mode t)
             (add-to-list 'company-backends 'company-ac-php-backend )))


;; Regenerate PHP ctags whenever emacs is idle for 5 hours.
(defun jv-redo-php-tags (path)
  (switch-to-buffer (find-file-noselect path))
  (ac-php-remake-tags-all))

(when (and
       (bound-and-true-p jv-php-path)
       (not (boundp 'jv-ac-tags-timer)))
  (setq jv-ac-tags-timer
        (run-with-idle-timer
         (* 60 60 5)
         t
         (lambda ()
           (message "Remaking PHP tags on a timer")
           (if (listp jv-php-path)
               (dolist (path jv-php-path) (jv-redo-php-tags path))
             (jv-redo-php-tags jv-php-path))))))

;; Mustache mode.
(require 'mustache-mode)
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))

;; SCSS mode.
;; Requires `sass` to be on $PATH.
(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook (lambda() (flymake-mode t)))

;; Golang

;; Loads go-oracle. Unavaible in MELPA.
;; (load-file "path/to/oracle.el")

;; Sets up formatting on save and compile, customizes the compile command.
(defun my-go-mode-hook ()
  (go-eldoc-setup)

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

  ;; Use go-imports as a replacement for go-fmt
  (setq gofmt-command "goimports")

  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (setq compile-command "go generate && go build -v && go test -v && go vet"))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Git
;; Find files quickly in a repository.
(global-set-key (kbd "C-x f") 'find-file-in-project)
