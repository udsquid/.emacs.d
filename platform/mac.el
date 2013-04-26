;; Change option to SUPER, command to META, fn to HYPER,
;; and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(set-face-attribute 'default nil :font "DejaVu Sans Mono-15")

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(provide 'mac)
