;; Highlight current line
(global-hl-line-mode 1)
;; Customize background color of highlighted line
(set-face-background 'hl-line "#222222")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

(provide 'appearance)
