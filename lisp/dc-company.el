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

  (add-hook 'after-init-hook 'global-company-mode)

  (use-package helm-company
    :disabled t
    :config
    (define-key company-mode-map (kbd "C-'") 'helm-company)
    (define-key company-active-map (kbd "C-'") 'helm-company)))

(provide 'dc-company)
