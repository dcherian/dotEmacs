(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq custom-safe-themes t)
(setq solarized-use-less-bold t
      solarized-use-more-italic nil
      solarized-use-variable-pitch nil)
(load-theme 'solarized-light)

(use-package circadian
  :ensure t
  :demand
  :config
  (setq circadian-themes '(("8:00" . solarized-light)
                           ("19:30" . solarized-dark)))
  (circadian-setup))

(setq-default line-spacing 6)
(setq x-underline-at-descent-line nil)

;; modeline options
(use-package smart-mode-line
  :ensure t
  :disabled
  :config
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup)
  (smart-mode-line-enable))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  ;; (moody-replace-vc-mode)
  )

(use-package auto-dim-other-buffers
  :disabled
  :config
  ;;  prevents helm from getting screwed up
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (add-hook 'after-init-hook (lambda ()
			       (auto-dim-other-buffers-mode t))))

(defun dc/theme-changes ()
  (interactive)
  (sml/setup)
  (smart-mode-line-enable)
  (dc/org-theme)

  ;; font changes
  (set-face-attribute 'font-lock-constant-face nil
		      :bold nil)

  (set-face-attribute 'font-lock-builtin-face nil
		      :foreground "#b58900"
		      :bold nil)

  (set-face-attribute 'default nil
		      :foreground "#21505a"
		      :family "ubuntu mono"
		      :height 140)

  (set-face-attribute 'tooltip nil
		      :inherit 'default
		      :foreground "#586e75"
		      :background "#eee8d5")

  (set-face-attribute 'vhl/default-face nil
		      :foreground "#fdf6e3"
		      :background "#d33682"))

(provide 'dc-theme)
