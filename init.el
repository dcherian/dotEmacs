(package-initialize)


(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/eq_waves/notes/paper/paper.org")))
 '(package-selected-packages
   (quote
    (restart-emacs ob-ipython ein matlab paredit avy no-littering helm-projectile projectile goto-last-change helm-describe-modes helm-ls-git yasnippet ox-nikola multiple-cursors helm-ag adaptive-wrap hungry-delete aggressive-indent helm-flx helm-fuzzier helm-swoop expand-region exec-path-from-shell matlab-mode crux ws-butler wc-mode volatile-highlights visual-fill-column use-package undo-tree solarized-theme smart-mode-line org-ref org-bullets magit comment-dwim-2)))
 '(safe-local-variable-values
   (quote
    ((eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1))
     (org-latex-table-scientific-notation)
     (org-latex-hyperref-template)
     (TeX-engine . xetex)
     (TeX-master . t)
     (org-image-actual-width . 600)
     (org-latex-remove-logfiles)))))

;; mac-specific
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  ;; (mac-auto-operator-composition-mode)
  )

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(defvar dc-bindings-map (make-keymap) "A keymap for custom bindings.")

;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-prefix-command 'dc/toggle-map)
(define-key ctl-x-map "t" 'dc/toggle-map)
(define-key dc/toggle-map "c" #'column-number-mode)
(define-key dc/toggle-map "l" #'toggle-truncate-lines)
(define-key dc/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key dc/toggle-map "w" #'whitespace-mode)

(require 'dc-theme)
(when window-system
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(setq inhibit-startup-message t
      initial-scratch-message ""
      inhibit-splash-screen t
      visible-bell nil
      ring-bell-function 'ignore
      sentence-end-double-space nil
      scroll-margin 3
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

(fset 'yes-or-no-p 'y-or-n-p)

(setq save-interprogram-paste-before-kill t
      select-enable-primary t
      select-enable-clipboard t)

;; enable built-in modes
(desktop-save-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(blink-cursor-mode 0)
(display-time-mode t)
(electric-pair-mode t)
(global-visual-line-mode t)
(global-hl-line-mode 1)
(global-subword-mode 1)
(global-prettify-symbols-mode t)
(windmove-default-keybindings)

;; diminsh some built-in modes
(diminish 'auto-revert-mode)
(diminish 'visual-line-mode)
(diminish 'subword-mode)
(diminish 'abbrev-mode)

;; global key bindings
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-xw" 'delete-frame)
(global-set-key "\C-c\C-r" 'eval-region)
(global-set-key "\C-c\C-b" 'eval-buffer)
(global-set-key (kbd "C-.") 'just-one-space)
(global-set-key (kbd "M-&") 'replace-string)
(global-set-key (kbd "M-*") 'replace-regexp)
(global-set-key (kbd "M-9") 'imenu)

(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f6>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

;; use aspell
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)

					; (define-key fortran-mode-map (kbd "C-c C-c") 'compile)

(use-package paredit
  :disabled t
  :config
  (paredit-mode))

(use-package wc-mode
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer)
	 :map isearch-mode-map
	 ("C-'" . avy-isearch)))

(use-package multiple-cursors
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-c m c" . mc/edit-lines)
	      ;; Remember `er/expand-region' is bound to M-2!
	      ("M-3" . mc/mark-next-like-this)
	      ("M-4" . mc/mark-previous-like-this)
	      ("M-1" . mc/mark-all-like-this-dwim)
	      ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package adaptive-wrap
  :ensure t
  :config
  (adaptive-wrap-prefix-mode 1))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode t))

(use-package goto-last-change
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-x x" . goto-last-change)))

(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (set-face-attribute 'vhl/default-face nil
		      :foreground nil
		      :background nil
		      :inherit 'isearch)
  (volatile-highlights-mode t))

(use-package undo-tree
  :demand t
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package expand-region
  :ensure t
  :bind (:map dc-bindings-map
	      ("M-2" . er/expand-region)))

(use-package yasnippet
  :ensure t
  :bind (:map dc-bindings-map
	      ("M-0 n" . yas-new-snippet)
	      ("M-0 s" . yas-insert-snippet)
	      ("M-0 v" . yas-visit-snippet-file))
  :config
  (yas-minor-mode))

(use-package crux
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-a" . crux-move-beginning-of-line)
	      ("C-c s" . crux-transpose-windows)
	      ("C-c d" . crux-delete-file-and-buffer)
	      ("C-c r" . crux-rename-file-and-buffer)
	      ("C-S-RET" . crux-smart-open-line-above)
	      ("S-RET" . crux-smart-open-line)
	      ("C-^" . crux-top-join-lines)))

(use-package comment-dwim-2
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-;" . comment-dwim-2)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))
	projectile-sort-order 'access-time)

  (use-package helm-projectile
    :ensure t
    :bind (:map projectile-command-map
		("s" . helm-projectile-ag))
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm
	  projectile-switch-project-action 'helm-projectile-find-file
	  projectile-switch-project-action 'helm-projectile))

  (projectile-global-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ws-butler
  :ensure t
  :demand t
  :config
  (ws-butler-global-mode t))

(use-package writegood-mode
  :ensure t
  :load-path "~/.emacs.d/writegood-mode/"
  :bind (:map dc/toggle-map
	      ("g" . writegood-mode))
  :config
  (set-face-attribute 'writegood-passive-voice-face nil
		      :foreground "#d33682"
		      :underline nil)
  (set-face-attribute 'writegood-weasels-face nil
		      :foreground "#d33682"
		      :underline nil))

(use-package visual-fill-column
  :ensure t
  :demand t
  :bind (:map dc/toggle-map
	      ("f" . visual-fill-column-mode))
  :init
  (setq visual-fill-column-center-text t
	visual-fill-column-width 90)
  (advice-add 'text-scale-adjust :after
	      #'visual-fill-column-adjust)
  (global-visual-fill-column-mode))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup))

(use-package magit
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-x g" . magit-status)
	      ("C-x l" . magit-log-buffer-file))
  :config
  ;; set untracked files to not be visible by default
  ;; modified from http://emacs.stackexchange.com/questions/20754/change-the-default-visibility-of-a-magit-section
  (defun local-magit-initially-hide-untracked (section)
    (and (not magit-insert-section--oldroot)
	 (eq (magit-section-type section) 'untracked)
	 'hide))
  (defun local-magit-initially-hide-stashes (section)
    (and (not magit-insert-section--oldroot)
	 (eq (magit-section-type section) 'stashes)
	 'hide))

  (add-hook 'magit-section-set-visibility-hook
	    'local-magit-initially-hide-untracked)
  (add-hook 'magit-section-set-visibility-hook
	    'local-magit-initially-hide-stashes)
  (magit-auto-revert-mode)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

(use-package discover-my-major
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-h C-m" . discover-my-major)
	      ("C-h M-m" . discover-my-mode)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(require 'dc-org)
(require 'dc-helm)
(require 'dc-company)
(require 'dc-comint)
(require 'dc-matlab)
(require 'dc-latex) ; (require 'dc-comint)
(require 'dc-editing)
(require 'dc-website)
(require 'dc-python)

;; do my keybindings
(define-minor-mode dc-bindings-mode
  "A mode that activates dc-bindings."
  :init-value t
  :lighter " dc-keys"
  :keymap dc-bindings-map)

(defun dc-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
   Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'dc-bindings-mode)
    (let ((mykeys (assq 'dc-bindings-mode minor-mode-map-alist)))
      (assq-delete-all 'dc-bindings-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))
(add-hook 'after-load-functions 'dc-keys-have-priority)

;; Garbage collector - decrease threshold to 15 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 15 1024 1024))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
