(package-initialize)

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)
(setq use-package-verbose t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-minimum-reported-time 0.05)
(require 'use-package)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0 t)

;; from https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(set-variable 'package-archives
	      `(("gnu" . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")))

(setq tls-checktrust t)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

(use-package restart-emacs
  :ensure)

(use-package whitespace
  :config
  (setq-default show-trailing-whitespace nil)
  (defun no-trailing-whitespace ()
    (setq show-trailing-whitespace nil))
  (add-hook 'minibuffer-setup-hook
	    'no-trailing-whitespace)
  (add-hook 'eww-mode-hook
	    'no-trailing-whitespace)
  (add-hook 'ielm-mode-hook
	    'no-trailing-whitespace)
  (add-hook 'gdb-mode-hook
	    'no-trailing-whitespace)
  (add-hook 'help-mode-hook
	    'no-trailing-whitespace))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; mac-specific
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  ;; (mac-auto-operator-composition-mode)
  )

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

(use-package transpose-frame
  :ensure t
  :bind (:map dc/toggle-map
	      ("f" . transpose-frame)))

(require 'dc-theme)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; from https://ogbe.net/emacsconfig.html
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

(setq inhibit-startup-message t
      initial-scratch-message ""
      inhibit-splash-screen t
      visible-bell nil
      ring-bell-function 'ignore
      sentence-end-double-space nil
      scroll-margin 3
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      split-height-threshold 100
      split-width-threshold 120)

;; smoooth - scrolling?
;; (setq scroll-margin 1
;;       scroll-conservatively 4
;;       scroll-up-aggressively 0.1
;;       scroll-down-aggressively 0.1)

(use-package smooth-scrolling
  :ensure
  :config
  (smooth-scrolling-mode t))

(fset 'yes-or-no-p 'y-or-n-p)

(setq save-interprogram-paste-before-kill t
      select-enable-primary t
      select-enable-clipboard t)

(setq tab-always-indent 'complete)

(setq tramp-default-method "ssh")

;; enable built-in modes
(desktop-save-mode 1)
(delete-selection-mode 1)

(save-place-mode 1)
(blink-cursor-mode 0)
(display-time-mode t)
(electric-pair-mode nil)
(global-visual-line-mode t)
(global-hl-line-mode 1)
(global-subword-mode 1)
(global-prettify-symbols-mode t)
(windmove-default-keybindings)
(which-function-mode t)

;; diminsh some built-in modes
(diminish 'auto-revert-mode)
(diminish 'visual-line-mode)
(diminish 'subword-mode)
(diminish 'abbrev-mode)

;; global key bindings
(global-set-key "\C-xw" 'delete-frame)
(global-set-key "\C-c\C-r" 'eval-region)
(global-set-key "\C-c\C-b" 'eval-buffer)
(global-set-key (kbd "C-.") 'just-one-space)
(global-set-key (kbd "M-&") 'replace-string)
(global-set-key (kbd "M-*") 'replace-regexp)
;; below works even when kill-this-buffer doesn't
(global-set-key (kbd "C-x k")
		(lambda () (interactive) (kill-buffer (current-buffer))))

(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)

(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f7>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f8>") 'gud-finish) ;; equiv matlab step out

;; use aspell
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)

					; (define-key fortran-mode-map (kbd "C-c C-c") 'compile)

(use-package paredit
  :disabled t
  :config
  (paredit-mode))

(use-package beginend
  :ensure
  :diminish beginend-global-mode
  :config
  (beginend-global-mode))

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
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
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
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (set-face-attribute 'vhl/default-face nil
		      :foreground "#fdf6e3"
		      :background "#d33682"))

(use-package undo-tree
  :demand t
  :ensure t
  :diminish undo-tree-mode
  :bind (:map dc-bindings-map
	      ("M--" . undo-tree-undo)
	      ("M-=" . undo-tree-redo)
	      ("M-u" . undo-tree-visualize))
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
  (yas-global-mode))

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
    :bind (:map dc-bindings-map
		("C-c C-f" . helm-projectile-find-file-dwim)
		:map projectile-command-map
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
	visual-fill-column-width 100)
  (advice-add 'text-scale-adjust :after
	      #'visual-fill-column-adjust))

(use-package magit
  :ensure t
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-after-change)
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
  (setq vc-handled-backends '(SVN Hg)))

(use-package discover-my-major
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-h C-m" . discover-my-major)
	      ("C-h M-m" . discover-my-mode)))

(use-package dumb-jump
  :ensure
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)
  :init (dumb-jump-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-local-then-key-order)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(require 'dc-org)
(require 'dc-helm)
(require 'dc-company)
(require 'dc-comint)
(require 'dc-latex) ; (require 'dc-comint)
(require 'dc-editing)
(require 'dc-parens)
(require 'dc-website)
(require 'dc-python)
(require 'dc-matlab)
(require 'ox-ipynb)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-indent-mode t)
 '(global-aggressive-indent-mode t)
 '(package-selected-packages
   (quote
    (company-jedi fancy-narrow helm-org-rifle lua-mode helm-ext company helm-unicode helm-descbinds which-key discover-my-major restart-emacs ob-ipython ein matlab paredit avy no-littering helm-projectile projectile goto-last-change helm-describe-modes helm-ls-git yasnippet ox-nikola multiple-cursors helm-ag adaptive-wrap hungry-delete aggressive-indent helm-flx helm-fuzzier helm-swoop expand-region exec-path-from-shell matlab-mode crux ws-butler wc-mode volatile-highlights visual-fill-column use-package undo-tree solarized-theme smart-mode-line org-ref org-bullets magit comment-dwim-2)))
 '(safe-local-variable-values
   (quote
    ((org-publish-use-timestamps-flag)
     (eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1))
     (org-latex-table-scientific-notation)
     (org-latex-hyperref-template)
     (TeX-engine . xetex)
     (TeX-master . t)
     (org-image-actual-width . 600)
     (org-latex-remove-logfiles))))
 '(yas-global-mode t))
