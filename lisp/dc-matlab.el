;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

(defvar default-fill-column 200)

;; needs to be matlab not matlab-mode!
;; this is because matlab.el is present, not matlab-mode.el
(require 'matlab)

(defun dc/matlab-shell-other-window ()
  (interactive)
  (other-window 1)
  (matlab-shell))

(setq matlab-shell-command "matlab"
      matlab-indent-function-body t
      matlab-functions-have-end t
      matlab-verify-on-save-flag nil
      matlab-shell-command-switches '("-nodesktop -nosplash")
      matlab-mode-verify-fix-functions nil
      matlab-shell-history-file "~/.matlab/R2016b/history.m")

(add-hook 'matlab-shell-hook 'comint-read-input-ring)

(bind-key "C-c C-m" 'matlab-shell matlab-mode-map)
(bind-key "C-c C-c" 'matlab-shell-run-region-or-line matlab-mode-map)
(bind-key "C-c C-a" 'matlab-shell-run-cell matlab-mode-map)
(bind-key "C-c C-o" 'dc/matlab-shell-other-window matlab-mode-map)

(provide 'dc-matlab)
