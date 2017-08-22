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

(set-face-attribute 'default nil
		    :family "mononoki"
		    :height 130)

(setq-default line-spacing 6)

(set-face-attribute 'tooltip nil
		    :inherit 'default
		    :foreground "#586e75"
		    :background "#eee8d5")

;; modeline options
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup)
  (smart-mode-line-enable))

;; powerline
(use-package powerline
  :ensure t
  :disabled t
  :config
  (powerline-default-theme))

;; spaceline
(use-package spaceline-config
  :ensure spaceline
  :disabled t
  :config

  (spaceline-helm-mode)
  (use-package spaceline-all-the-icons
    :after spaceline
    :ensure t
    :config

    (use-package all-the-icons
      :ensure t)

    (spaceline-all-the-icons-theme)
    (setq spaceline-all-the-icons-separator-type 'arrow))

  (set-face-attribute 'spaceline-highlight-face nil
		      :background "#93a1a1"))

(provide 'dc-theme)
