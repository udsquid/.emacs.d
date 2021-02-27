;; set 'command' as META key, 'option/alt' as SUPER key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; switch 'C-x' & 'C-t' keys
(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\C-t ?\C-x)

;; setup package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-auto-character-face-perc 20)
 '(highlight-indent-guides-method 'bitmap)
 '(package-selected-packages
   '(visual-fill-column org-bullets exec-path-from-shell vterm org org-tempo magit ivy-rich restclient smartparens ws-butler anzu perspective doom-modeline all-the-icons multiple-cursors dashboard highlight-indent-guides which-key expand-region helm helpful avy cyberpunk-theme use-package))
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
  :init
  (load-theme 'cyberpunk t))

;; turn off any window widgets
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup font
(set-face-attribute 'default nil
		    :font    "DejaVu Sans Mono"
		    :height  180)

;; cursor jumping package
(use-package avy
  :bind ("M-o" . avy-goto-char))

;; highlight current line
(global-hl-line-mode t)

;; show column number
(setq column-number-mode t)

;; better help system
(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))

;; powerful search framework
(use-package helm
  :bind (("M-x"             . helm-M-x)
	 ([remap find-file] . helm-find-files)
	 ([remap occur]     . helm-occur))
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
  :bind (("s-h" . er/expand-region)
	 ("s-i" . er/mark-inside-quotes)
	 ("s-o" . er/mark-outside-quotes)))

;; show hint for key bindings
(use-package which-key
  :config
  (which-key-mode))

;; highlight indentation levels
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; show recent files and projects
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; edit multiple lines at once
(define-prefix-command 'mc-map)
(global-set-key (kbd "s-m") 'mc-map)
(use-package multiple-cursors
  :bind (("s-t"   . mc/mark-next-like-this)
	 ("s-w"   . mc/mark-next-like-this-word)
	 ("s-m e" . mc/mark-more-like-this-extended)
	 ("s-m l" . mc/edit-lines)
	 ("s-m a" . mc/mark-all-like-this)
	 ("s-m r" . mc/mark-all-in-region)
	 ("s-m y" . yank-rectangle)))

;; pretty icons
;; remember to run command: all-the-icons-install-fonts
(use-package all-the-icons)

;; fancy but minimal mode-line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq vc-display-status nil)
  (advice-add #'doom-modeline-update-vcs-text :override #'ignore)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

;; manage buffers in project perspective
(use-package perspective
  :init
  (persp-mode)
  :bind (("C-x b" . persp-switch-to-buffer*)
	 ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-sort 'access))

;; show the matches while in search/replace
(use-package anzu
  :init
  (global-anzu-mode)
  :bind (([remap query-replace] . 'anzu-query-replace)
	 ([remap query-replace-regexp] . 'anzu-query-replace-regexp)))

;; smart delete whitespaces & blank lines
(use-package ws-butler
  :init
  (ws-butler-global-mode))

;; smart way to manipulate parenthesis
(use-package smartparens
  :bind (("M-[" . sp-backward-sexp)
	 ("M-]" . sp-forward-sexp))
  :init
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode))

;; HTTP REST tool
(use-package restclient)

;; better git client
(use-package magit)

;; better note app
(defun org-setup-font ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ +\\([-*]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1        . 1.5)
                  (org-level-2        . 1.4)
                  (org-level-3        . 1.3)
                  (org-level-4        . 1.2)
                  (org-level-5        . 1.2)
                  (org-level-6        . 1.2)
                  (org-level-7        . 1.2)
                  (org-level-8        . 1.2)
		  (org-document-title . 2.0)))
    (set-face-attribute (car face) nil
			:family "Arial Rounded MT Bold"
			:weight 'regular
			:height (cdr face)))

  ;; set faces for markups & code blocks
  (set-face-attribute 'org-verbatim nil
		      :inherit '(shadow fixed-pitch)
		      :foreground "orchid2")
  (set-face-attribute 'org-code nil
		      :inherit '(shadow)
		      :foreground "light green")
  (set-face-attribute 'org-block nil
		      :background nil)
  )

(defun org-setup-mode ()
  (visual-line-mode t)
  (org-indent-mode))

(use-package org
  :bind ("C-c C-." . org-insert-structure-template)
  :hook (org-mode . org-setup-mode)
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded nil)
  (org-setup-font)

  ;; enable language execution
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python .t )
				 (shell . t)))

  ;; code block generator
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun org-setup-visual ()
  (setq visual-fill-column-width 80)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-setup-visual))

;; better terminal
(use-package vterm
  :commands vterm
  :bind
  ("C-c C-x" . vterm-copy-mode)
  (:map vterm-mode-map
	("M-i"     . vterm-beginning-of-line)
	("M-p"     . vterm-previous-prompt)
	("M-n"     . vterm-next-prompt))
  :config
  (setq vterm-shell "/usr/local/bin/bash")
  (setq vterm-max-scrollback 10000)
  (unbind-key "M-w" vterm-mode-map)
  (unbind-key "M-p" vterm-mode-map)
  (unbind-key "M-n" vterm-mode-map))

;; copy environment variables
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
