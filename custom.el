(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auth-source-save-behavior nil)
 '(auto-dim-other-buffers-mode nil)
 '(blink-cursor-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(electric-indent-mode t)
 '(fci-rule-color "#073642")
 '(global-aggressive-indent-mode nil)
 '(global-undo-tree-mode t)
 '(helm-flx-mode t)
 '(helm-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(ivy-mode t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(beacon apropospriate-theme super-save wucuo counsel-projectile company-lsp lsp-ui lsp-mode citeproc citeproc-orgref company-quickhelp moody ox-reveal org-reveal minions company-statistics circadian cdlatex counsel outshine yaml-mode auto-dim-other-buffers matlab-mode-elpa ox-latex beginend smooth-scrolling helm-dash dumb-jump org-sticky-header spaceline-all-the-icons spaceline all-the-icons major-mode-icons pydoc kaomoji elpy helm-pydoc origami transpose-frame w3 org-edit-latex rainbow-mode markdown-mode company-jedi fancy-narrow helm-org-rifle lua-mode helm-ext helm-unicode helm-descbinds which-key discover-my-major restart-emacs ob-ipython ein matlab paredit no-littering helm-projectile goto-last-change helm-describe-modes helm-ls-git ox-nikola multiple-cursors helm-ag hungry-delete helm-flx helm-fuzzier helm-swoop expand-region exec-path-from-shell matlab-mode crux ws-butler wc-mode volatile-highlights use-package undo-tree smart-mode-line org-bullets magit comment-dwim-2))
 '(projectile-mode t nil (projectile))
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c8805d801780")
     (60 . "#bec073400bc0")
     (80 . "#b58900")
     (100 . "#a5008e550000")
     (120 . "#9d0091000000")
     (140 . "#950093aa0000")
     (160 . "#8d0096550000")
     (180 . "#859900")
     (200 . "#66aa9baa32aa")
     (220 . "#57809d004c00")
     (240 . "#48559e556555")
     (260 . "#392a9faa7eaa")
     (280 . "#2aa198")
     (300 . "#28669833af33")
     (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
