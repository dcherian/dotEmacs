(setq custom-safe-themes t)
(setq solarized-use-less-bold t
      solarized-use-more-italic nil
      solarized-use-variable-pitch nil)
(load-theme 'solarized-light)

(set-face-attribute 'font-lock-constant-face nil
		    :inherit 'font-lock-constant-face
		    :bold nil)

(set-face-attribute 'font-lock-builtin-face nil
		    :foreground "#b58900"
		    :bold nil)

(set-face-attribute 'default nil :height 160)

(setq-default line-spacing 6)

(set-face-attribute 'tooltip nil
		    :inherit 'default
		    :foreground "#586e75"
		    :background "#eee8d5")
(provide 'dc-theme)
