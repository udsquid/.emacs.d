;;; Basic settings
;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
;; Are we on a linux?
(setq is-linux (equal system-type 'gnu/linux))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq user-platform-dir
      (concat user-emacs-directory "platform/"))
(setq user-setup-dir
      (concat user-emacs-directory "setup/"))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path user-platform-dir)
(add-to-list 'load-path user-setup-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Use SSH as tramp method
(setq tramp-default-method "ssh")

;;; Prepare packages
;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'color-theme melpa)
   (cons 'magit melpa)
   (cons 'undo-tree melpa)
   (cons 'dired-details melpa)
   (cons 'shell-command melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-ido)
(require 'setup-hippie)

;; Load packages
(require 'expand-region)
(require 'visual-regexp)
(require 'elisp-slime-nav)

;; Beautiful look
(require 'color-theme)
(add-to-list 'custom-theme-load-path
	     (concat user-emacs-directory "repo/"))
(load-theme 'tangotango t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name ".places" user-emacs-directory))

;; Quick jump to anywhere in Emacs
(autoload 'ace-jump-mode "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;;; Misc. settings
;; Decide how the Emacs looks like
(require 'appearance)
;; Setup key bindings
(require 'key-bindings)

;; load Mac-specific settings when on Mac system
(when is-mac (require 'mac))
;; load Linux-specific settings when on Linux system
(when is-linux (require 'linux))
