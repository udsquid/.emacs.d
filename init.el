(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\C-t ?\C-x)

(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-f") 'quick-switch-buffer)
