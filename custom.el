(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(jupyter zmq gcmh ox-clip quelpa-use-package quelpa ox-hugo forge beacon apropospriate-theme super-save wucuo counsel-projectile citeproc citeproc-orgref company-quickhelp moody ox-reveal org-reveal minions company-statistics circadian cdlatex counsel yaml-mode auto-dim-other-buffers matlab-mode-elpa ox-latex beginend smooth-scrolling helm-dash dumb-jump org-sticky-header spaceline-all-the-icons spaceline all-the-icons major-mode-icons pydoc kaomoji elpy helm-pydoc origami transpose-frame w3 org-edit-latex rainbow-mode markdown-mode company-jedi fancy-narrow helm-org-rifle lua-mode helm-ext helm-unicode helm-descbinds which-key discover-my-major restart-emacs ob-ipython ein matlab paredit no-littering helm-projectile goto-last-change helm-describe-modes helm-ls-git ox-nikola multiple-cursors helm-ag hungry-delete helm-flx helm-fuzzier helm-swoop expand-region exec-path-from-shell matlab-mode crux ws-butler wc-mode volatile-highlights use-package undo-tree smart-mode-line org-bullets magit comment-dwim-2))

 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-beamer-export-to-latex t t)
     (eval add-hook 'after-save-hook org-beamer-export-to-pdf t t)
     (eval add-hook 'after-save-hook
	   '(org-beamer-export-to-pdf t)
	   t t)
     (eval add-hook 'after-save-hook 'org-beamer-export-to-pdf t t)
     (eval add-hook 'after-save-hook 'org-latex-export-to-latex t t)
     (eval add-hook 'after-save-hook 'org-html-export-to-latex t t)
     (eval add-hook 'after-save-hook 'org-html-export-to-html t t)
     (org-image-actual-width . 300)
     (org-publish-use-timestamps-flag)
     (eval when
	   (fboundp 'rainbow-mode)
	   (rainbow-mode 1))
     (org-latex-table-scientific-notation)
     (org-latex-hyperref-template)
     (TeX-engine . xetex)
     (TeX-master . t)
     (org-image-actual-width . 600)
     (org-latex-remove-logfiles)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
