(use-package company
  :diminish company-mode
  :ensure t
  :config
  ;; Enable company-mode globally.
  (global-company-mode)
  ;; Except when you're in term-mode.
  (setq company-global-modes '(not term-mode))

  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
	company-dabbrev-ignore-case nil)
  (setq company-idle-delay 0.6)

  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-backends
	'((company-files company-elisp company-semantic company-files company-dabbrev-code company-gtags company-etags company-keywords company-dabbrev)))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package helm-company
  :disabled t
  :config
  (define-key company-mode-map (kbd "C-'") 'helm-company)
  (define-key company-active-map (kbd "C-'") 'helm-company))

(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  (setq company-math-allow-latex-symbols-in-faces t)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  )

;; rank completion candidates by statistics of use
(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

(provide 'dc-company)
