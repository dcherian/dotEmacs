(require 'ob-ipython)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (emacs-lisp . t)))

;; don’t prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
(setq python-indent-offset 4)
(setq python-shell-prompt-detect-failure-warning nil)

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(defun darya-setup ()
  (message "Setting python paths for darya.")
  (setq python-shell-interpreter "~/anaconda3/bin/python")
  (setq-default org-babel-python-command "~/anaconda3/bin/python")
  (setq-default ob-ipython-command "~/anaconda3/bin/jupyter")
  (setq exec-path (append exec-path '("~/anaconda3/bin/"))))

(if (string-equal system-name "darya")
    (darya-setup))

(use-package ob-ipython
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-M-i" . ob-ipython-inspect)
	      ("C-c t" . python-shell-switch-to-shell)
	      ("C-<tab>" . org-hide-block-toggle-maybe)))

(provide 'dc-python)
