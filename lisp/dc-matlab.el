;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

(defvar default-fill-column 100)

(use-package matlab-mode
  :ensure t
  :mode "\\.m$"
  :bind (:map matlab-mode-map
	      ("C-c C-m" . matlab-shell)
	      ("C-c C-c" . matlab-shell-run-region-or-line)
	      ("C-c C-a" . matlab-shell-run-cell)
	      ("C-c C-o" . dc/matlab-shell-other-window)))

(setq-default matlab-shell-command "matlab"
	      matlab-indent-function-body t   ; if you want function bodies indented
	      matlab-functions-have-end t
	      matlab-verify-on-save-flag nil
	      matlab-shell-command-switches '("-nodesktop -nosplash")
	      matlab-mode-verify-fix-functions nil
	      matlab-shell-history-file "~/.matlab/R2016b/history.m")

(defun dc/matlab-shell-other-window ()
  (interactive)
  (other-window 1)
  (matlab-shell))

(add-hook 'matlab-shell-hook 'comint-read-input-ring)

(provide 'dc-matlab)
