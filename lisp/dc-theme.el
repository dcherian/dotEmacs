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

;; modeline options
(use-package smart-mode-line
  :ensure t
  :disable t
  :config
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup))

;; powerline
(use-package powerline
  :ensure t
  :disable t
  :config
  (powerline-default-theme))

;; spaceline
(use-package spaceline
  :ensure t
  :config
  (use-package spaceline-all-the-icons
    :after spaceline
    :ensure t
    :config

    (use-package all-the-icons
      :ensure t)

    (spaceline-all-the-icons-theme)))

(provide 'dc-theme)
