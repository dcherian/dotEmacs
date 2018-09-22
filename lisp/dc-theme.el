(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq custom-safe-themes t)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)
  (setq solarized-use-less-bold t
      solarized-use-more-italic nil
      solarized-use-variable-pitch nil)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package circadian
  :ensure t
  :demand
  :config
  (setq circadian-themes '(("8:00" . solarized-light)
                           ("19:30" . solarized-dark)))
  (circadian-setup))

(setq-default line-spacing 5)
(setq x-stretch-cursor nil)
(setq x-underline-at-descent-line t)

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
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq moody-mode-line-height 28))

(use-package auto-dim-other-buffers
  :disabled
  :config
  ;;  prevents helm from getting screwed up
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (add-hook 'after-init-hook (lambda ()
			       (auto-dim-other-buffers-mode t))))

(defun dc/light-theme-changes ()
  (interactive)
  (set-face-attribute 'default nil
		      :foreground "#21505a"
		      :family "fira mono"
		      :height 120)
  (dc/theme-changes))

(defun dc/dark-theme-changes ()
  (interactive)
  (set-face-attribute 'default nil
		      :foreground "#839496"
		      :family "fira mono"
		      :height 120)
  (dc/theme-changes))

(defun dc/theme-changes ()
  (interactive)
  (dc/org-theme)

  ;; font changes
  (set-face-attribute 'font-lock-constant-face nil
		      :bold nil)

  (set-face-attribute 'font-lock-builtin-face nil
		      :foreground "#b58900"
		      :bold nil)

  (set-face-attribute 'tooltip nil
		      :inherit 'default
		      :foreground "#586e75"
		      :background "#eee8d5")

  (set-face-attribute 'vhl/default-face nil
		      :foreground "#fdf6e3"
		      :background "#d33682")

  (set-face-attribute 'lsp-face-highlight-read nil
		      :foreground "#d33682"
		      :background nil)

  (set-face-attribute 'lsp-face-highlight-write nil
		      :foreground "#b58900"
		      :background nil))

(provide 'dc-theme)
