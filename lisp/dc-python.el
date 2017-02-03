(use-package ob-ipython
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-M-i" . ob-ipython-inspect))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     (emacs-lisp . t)))

  ;; don’t prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook ‘org-display-inline-images ‘append))

(provide 'dc-python)
