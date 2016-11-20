;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :bind (:map matlab-mode-map
	      ("C-c C-m" . matlab-shell))
  :config
  (setq matlab-shell-command "/Applications/MATLAB_R2016a.App/bin/matlab"
	matlab-indent-function-body t   ; if you want function bodies indented
	matlab-verify-on-save-flag nil
	matlab-shell-command-switches '("-nodesktop -nosplash"))

  (defun dc/matlab-shell-other-window ()
    (interactive)
    (other-window)
    (matlab-shell))

  (comint-read-input-ring))

(provide 'dc-matlab)
