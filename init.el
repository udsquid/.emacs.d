;; set 'command' as META key, 'option/alt' as SUPER key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; switch 'C-x' & 'C-t' keys
(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\C-t ?\C-x)

;; setup package repository
;; copy from: https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; install 'use-package', a package to tidy up your package configurations
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-auto-character-face-perc 20)
 '(highlight-indent-guides-method 'bitmap)
 '(package-selected-packages
   '(smartparens ws-butler anzu perspective all-the-icons doom-modeline multiple-cursors dashboard highlight-indent-guides which-key expand-region helm helpful avy cyberpunk-theme use-package))
 '(persp-mode-prefix-key [8388720])
 '(persp-state-default-file (concat user-emacs-directory ".persp")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; setup theme
(use-package cyberpunk-theme
  :ensure t
  :init
  (load-theme 'cyberpunk t))

;; turn off any window widgets
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup font
(add-to-list 'default-frame-alist '(font . "Hack-16"))

;; cursor jumping package
(use-package avy
  :ensure t
  :bind ("M-o" . avy-goto-char))

;; highlight current line
(global-hl-line-mode t)

;; show column number
(setq column-number-mode t)

;; better help system
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))

;; powerful search framework
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ([remap find-file] . helm-find-files))
  :init
  (helm-mode t))

;; disable auto save buffer
(setq auto-save-default nil)
;; disable backup file
(setq make-backup-files nil)

;; turn off newbie protection
(put 'narrow-to-region 'disabled nil)

;; move around between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; smart select region
(use-package expand-region
  :ensure t
  :bind (("s-h" . er/expand-region)
	 ("s-i" . er/mark-inside-quotes)
	 ("s-o" . er/mark-outside-quotes)))

;; show hint for key bindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; highlight indentation levels
(use-package highlight-indent-guides
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; show recent files and projects
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; edit multiple lines at once
(define-prefix-command 'mc-map)
(global-set-key (kbd "s-m") 'mc-map)
(use-package multiple-cursors
  :ensure t
  :bind (("s-t" . mc/mark-next-like-this)
	 ("s-w" . mc/mark-next-like-this-word)
	 ("s-m e" . mc/mark-more-like-this-extended)
	 ("s-m l" . mc/edit-lines)
	 ("s-m a" . mc/mark-all-like-this)
	 ("s-m r" . mc/mark-all-in-region)
	 ("s-m y" . yank-rectangle)))

;; pretty icons
;; remember to run command: all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; fancy but minimal mode-line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq vc-display-status nil)
  (advice-add #'doom-modeline-update-vcs-text :override #'ignore))

;; manage buffers in project perspective
(use-package perspective
  :ensure t
  :init
  (persp-mode)
  :bind (("C-x b" . persp-switch-to-buffer*)
	 ("C-x k" . persp-kill-buffer*))
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; show the matches while in search/replace
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode)
  :bind (([remap query-replace] . 'anzu-query-replace)
	 ([remap query-replace-regexp] . 'anzu-query-replace-regexp)))

;; smart delete whitespaces & blank lines
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode))

;; smart way to manipulate parenthesis
(use-package smartparens
  :ensure t
  :bind (("M-[" . sp-backward-sexp)
	 ("M-]" . sp-forward-sexp))
  :init
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode))

;; make Emacs as a programming IDE
(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "/usr/local/bin/python"))

;; --- handy custom keys ---

;; ### cursor moving ###
(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "M-i") 'back-to-indentation)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-N") 'scroll-up-command)
(global-set-key (kbd "M-P") 'scroll-down-command)

;; ### buffer & window ###
(global-set-key (kbd "M-w") 'other-window)

(global-unset-key (kbd "C-x C-p"))
(global-set-key (kbd "C-x C-p C-f") 'project-find-file)

;; quick switch between two recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-f") 'quick-switch-buffer)

;; ### editing ###
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "M-y") 'yank-pop)

(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-t") 'capitalize-word)

(global-set-key (kbd "M-z") 'undo)
