;;; Basic settings
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Setup load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path
	     (concat user-emacs-directory "platform/"))
(add-to-list 'load-path
	     (concat user-emacs-directory "setup/"))
(add-to-list 'load-path
	     (concat user-emacs-directory "site-lisp/"))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
;; Are we on a linux?
(setq is-linux (equal system-type 'gnu/linux))

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
   (cons 'ace-jump-mode melpa)
   (cons 'color-theme melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; color-theme
(require 'color-theme)
(add-to-list 'custom-theme-load-path
	     (concat user-emacs-directory "repo/"))
(load-theme 'tangotango t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name ".places" user-emacs-directory))

;; load Mac-specific settings when on Mac system
(when is-mac (require 'mac))
;; load Linux-specific settings when on Linux system
(when is-linux (require 'linux))
