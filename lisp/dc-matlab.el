;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

(use-package matlab-mode
  :ensure t
  :mode "\\.m$"
  :bind (:map matlab-mode-map
	      ("C-c C-m" . matlab-shell)
	      ("C-c C-c" . matlab-shell-run-region-or-line)
	      ("C-c C-a" . matlab-shell-run-cell)
	      ("C-c C-o" . dc/matlab-shell-other-window))
  :config
  (setq-default matlab-shell-command "/Applications/MATLAB_R2016a.App/bin/matlab"
		matlab-indent-function-body t   ; if you want function bodies indented
		matlab-functions-have-end t
		matlab-verify-on-save-flag nil
		matlab-shell-command-switches '("-nodesktop -nosplash")
		matlab-mode-verify-fix-functions nil)

  (defun dc/matlab-shell-other-window ()
    (interactive)
    (other-window 1)
    (matlab-shell))

  (add-hook 'matlab-shell-hook 'comint-read-input-ring))

(provide 'dc-matlab)
