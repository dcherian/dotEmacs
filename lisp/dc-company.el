(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case t)

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
    :config
    (define-key company-mode-map (kbd "C-'") 'helm-company)
    (define-key company-active-map (kbd "C-'") 'helm-company)))

(provide 'dc-company)
