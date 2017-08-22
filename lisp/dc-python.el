(require 'ob-ipython)

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(elpy-enable)
(setq elpy-rpc-backend "jedi")

(use-package pydoc
  :ensure t
  :disabled t)

(use-package company-jedi
  :ensure t
  :disabled t
  :config
  (defun dc/use-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))

  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'dc/use-company-jedi))

;; don’t prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
(setq-default python-indent-offset 4)
(setq python-shell-prompt-detect-failure-warning nil)

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(defun darya-setup ()
  (message "Setting python paths for darya.")
  (setq python-shell-interpreter "/home/deepak/anaconda3/bin/ipython")
  (setq python-shell-interpreter-args "--simple-prompt --pylab")
  (setq-default org-babel-python-command "/home/deepak/anaconda3/bin/ipython")
  (setq-default ob-ipython-command "/home/deepak/anaconda3/bin/jupyter")
  (setq-default ob-ipython-kernel-extra-args 'nil)
  (setq exec-path (append exec-path '("/home/deepak/anaconda3/bin/"))))

(if (string-equal system-name "darya")
    (darya-setup))

(defun python-shell-run-region-or-line ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (python-shell-send-region (mark) (point))
    (python-shell-send-region (point-at-bol) (point-at-eol))))

(defun dc-switch-to-python-shell ()
  (interactive)
  (recenter-top-bottom)
  (split-window-below -15)
  (other-window 1)
  (switch-to-buffer "*Python*"))

(bind-key (kbd "C-c C-c") 'python-shell-run-region-or-line python-mode-map)
(bind-key (kbd "C-c C-b") 'python-shell-send-buffer python-mode-map)

(use-package ob-ipython
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-c p" . python-shell-run-region-or-line)
	      ("s-i" . ob-ipython-inspect)
	      ("C-c t" . dc-switch-to-python-shell)
	      ("C-<tab>" . org-hide-block-toggle-maybe)))

(setq org-babel-default-header-args:ipython
      '(;;(:results . "output replace")
	;;(:session . "none")
	(:exports . "results")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "yes")
	(:eval . "never-export")))

;; from https://ekaschalk.github.io/post/prettify-mode/
(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
	   ;;	   ("def" .      #x2131)
	   ("not" .      #x2757)
	   ("in" .       #x2208)
	   ("not in" .   #x2209)
	   ("return" .   #x27fc)
	   ("yield" .    #x27fb)
	   ;; ("for" .      #x2200)
	   ;; Base Types
	   ;; ("int" .      #x2124)
	   ;; ("float" .    #x211d)
	   ;; ("str" .      #x1d54a)
	   ("True" .     #x1d54b)
	   ("False" .    #x1d53d)
	   ;; Mypy
	   ("dict" .     #x1d507)
	   ("List" .     #x2112)
	   ("Tuple" .    #x2a02)
	   ("Set" .      #x2126)
	   ("Iterable" . #x1d50a)
	   ("Any" .      #x2754)
	   ("Union" .    #x22c3)))))

(define-key elpy-mode-map (kbd "C-<up>") 'nil)
(define-key elpy-mode-map (kbd "C-<down>") 'nil)

(provide 'dc-python)
