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

(use-package auto-dim-other-buffers
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
		      :background "#eee8d5")
  ;;  prevents helm from getting screwed up
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (add-hook 'after-init-hook (lambda ()
			       (auto-dim-other-buffers-mode t))))

(defun dc/org-theme ()
  (interactive)
  ;; org-faces
  (set-face-attribute 'org-level-1 nil
		      :inherit 'outline-1 :height 1.25)
  (set-face-attribute 'org-level-2 nil
		      :inherit 'outline-2 :height 1.2)
  (set-face-attribute 'org-level-3 nil
		      :inherit 'outline-3 :height 1.15)
  (set-face-attribute 'org-link nil
		      :inherit 'org-link
		      :foreground nil) ; links are only underlined
  ;; footnotes shouldn't be highlighted
  (set-face-attribute 'org-footnote nil
		      :foreground nil
		      :underline nil
		      :inherit '(font-lock-comment-face org-foreground))
  (set-face-attribute 'org-checkbox nil
		      :inherit '(font-lock-comment-face)
		      :background nil
		      :weight 'light
		      :box nil)
  (set-face-attribute 'org-todo nil
		      :weight 'normal)
  (set-face-attribute 'org-done nil
		      :weight 'normal)
  (set-face-attribute 'org-block nil
		      :foreground nil
		      :background nil)
  (set-face-attribute 'org-target nil
		      :foreground "#586e75"
		      :background nil)
  (set-face-attribute 'org-table nil
		      :family "Ubuntu Mono"
		      :background nil)
  (set-face-attribute 'org-date nil
		      :foreground nil
		      :inherit 'org-link)
  (set-face-attribute 'org-latex-and-related nil
		      :foreground "#268bd2")
  (set-face-attribute 'org-tag nil
		      :height 0.7
		      :inherit '(font-lock-comment-face org-foreground))
  (set-face-attribute 'org-ref-cite-face nil
		      :inherit 'org-link
		      :foreground nil)
  (set-face-attribute 'org-ref-ref-face nil
		      :inherit 'org-ref-cite-face
		      :foreground nil)
  (set-face-attribute 'org-meta-line nil
		      :height 0.85))

(defun dc/theme-changes ()
  (interactive)
  (auto-dim-other-buffers-mode)
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
		      :family "mononoki"
		      :height 130)

  (set-face-attribute 'tooltip nil
		      :inherit 'default
		      :foreground "#586e75"
		      :background "#eee8d5"))

(dc/theme-changes)

;; modeline options
(use-package smart-mode-line
  :ensure t
  :demand
  :config
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup)
  (smart-mode-line-enable))

(provide 'dc-theme)
