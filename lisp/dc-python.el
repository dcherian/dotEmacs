;; elpy & jedi
(require 'elpy)

(setq elpy-modules '(;; elpy-module-highlight-indentation
		     elpy-module-sane-defaults
		     elpy-module-company
		     elpy-module-flymake
		     elpy-module-eldoc
		     ;; elpy-module-pyvenv
		     ;; elpy-module-yasnippet
		     ;; elpy-module-django
		     ))
(elpy-enable)
(setq elpy-get-info-from-shell nil)
(setq elpy-rpc-python-command "python")

(setq eldoc-idle-delay 1)

(use-package beacon
  :ensure)

;; (use-package ob-ipython
;;   :ensure t
;;   :bind (:map dc-bindings-map
;; 	      ("C-c p" . python-shell-run-region-or-line)
;; 	      ("s-i" . ob-ipython-inspect)
;; 	      ("C-c t" . dc-switch-to-python-shell)
;; 	      ("C-<tab>" . org-hide-block-toggle-maybe)
;; 	      :map org-mode-map
;; 	      ("C-c C-v C-k" . ob-ipython-kill-kernel)
;; 	      ("C-c C-v C-i" . ob-ipython-interrupt-kernel)
;; 	      :map inferior-python-mode-map
;; 	      ("C-c C-v C-k" . ob-ipython-kill-kernel)
;; 	      ("C-c C-v C-i" . ob-ipython-interrupt-kernel))
;;   :config
;;   (require 'scimax-org-babel-python)
;;   (require 'scimax-org-babel-ipython-upstream)

;;   (setq ob-ipython-buffer-unique-kernel nil)

;;   ;; unusuably slow
;;   ;; (add-to-list 'company-backends 'company-ob-ipython)
;;   )

(require 'quelpa-use-package)
(use-package jupyter
  :quelpa ((jupyter :fetcher github :repo "dzop/emacs-jupyter") :upgrade t)
  :bind (:map dc-bindings-map
	      ("C-c p" . python-shell-run-region-or-line)
	      ("C-c t" . jupyter-repl-pop-to-buffer)
	      ("C-<tab>" . org-hide-block-toggle-maybe)
	      :map org-mode-map
	      ;; ("C-c C-v C-k" . ob-ipython-kill-kernel)
	      ("C-c C-v C-i" . jupyter-repl-interrupt-kernel)
	      :map inferior-python-mode-map
	      ;; ("C-c C-v C-k" . ob-ipython-kill-kernel)
	      ("C-c C-v C-i" . jupyter-repl-interrupt-kernel))
  :config
  (set-face-attribute 'jupyter-repl-traceback nil
		      :background nil)

  (set-face-attribute 'jupyter-repl-input-prompt nil
		      :foreground "#b58900")

  (set-face-attribute 'jupyter-repl-output-prompt nil
		      :foreground "#dc322f")

  (org-babel-jupyter-override-src-block "python")

  (setq org-babel-default-header-args:jupyter-python
	'((:results . "none")
	  (:session . "py")
	  (:exports . "results")
	  (:kernel . "python3")
	  (:cache .   "no")
	  (:noweb . "yes")
	  (:hlines . "no")
	  (:tangle . "yes")
	  (:eval . "never-export"))))

(setq org-babel-default-header-args:ipython
      '((:results . "none")
	;;(:session . "none")
	(:exports . "results")
	(:cache .   "no")
	(:noweb . "yes")
	(:hlines . "no")
	(:tangle . "yes")
	(:eval . "never-export")))

;; don’t prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq-default python-indent-offset 4)
(setq python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(use-package pydoc
  :ensure t
  :disabled t)

(defun darya-setup ()
  (message "Setting python paths for darya.")
  (setq python-shell-interpreter "/home/deepak/anaconda3/bin/ipython")
  (setq python-shell-interpreter-args "--simple-prompt")
  (setq-default org-babel-python-command "/home/deepak/anaconda3/bin/jupyter")
  ;; (setq-default ob-ipython-command "/home/deepak/anaconda3/bin/jupyter")
  ;; (setq-default ob-ipython-kernel-extra-args 'nil)
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

;; ;; from https://ekaschalk.github.io/post/prettify-mode/
;; (add-hook
;;  'python-mode-hook
;;  (lambda ()
;;    (mapc (lambda (pair) (push pair prettify-symbols-alist))
;;          '(;; Syntax
;; 	   ;;	   ("def" .      #x2131)
;; 	   ("not" .      #x2757)
;; 	   ("in" .       #x2208)
;; 	   ("not in" .   #x2209)
;; 	   ;; ("return" .   #x27fc)
;; 	   ("yield" .    #x27fb)
;; 	   ;; ("for" .      #x2200)
;; 	   ;; Base Types
;; 	   ;; ("int" .      #x2124)
;; 	   ;; ("float" .    #x211d)
;; 	   ;; ("str" .      #x1d54a)
;; 	   ("True" .     #x1d54b)
;; 	   ("False" .    #x1d53d)
;; 	   ;; Mypy
;; 	   ("dict" .     #x1d507)
;; 	   ("List" .     #x2112)
;; 	   ("Tuple" .    #x2a02)
;; 	   ("Set" .      #x2126)
;; 	   ("Iterable" . #x1d50a)
;; 	   ("Any" .      #x2754)
;; 	   ("Union" .    #x22c3)))))

;; (define-key elpy-mode-map (kbd "C-<up>") 'nil)
;; (define-key elpy-mode-map (kbd "C-<down>") 'nil)

(define-key inferior-python-mode-map (kbd "C-c C-c")
  'ob-ipython-interrupt-kernel)

;; (advice-add 'elpy-shell--insert-and-font-lock
;;             :around (lambda (f string face &optional no-font-lock)
;;                       (if (not (eq face 'comint-highlight-input))
;;                           (funcall f string face no-font-lock)
;;                         (funcall f string face t)
;;                         (python-shell-font-lock-post-command-hook))))

;; (advice-add 'comint-send-input
;;             :around (lambda (f &rest args)
;;                       (if (eq major-mode 'inferior-python-mode)
;;                           (cl-letf ((g (symbol-function 'add-text-properties))
;;                                     ((symbol-function 'add-text-properties)
;;                                      (lambda (start end properties &optional object)
;;                                        (unless (eq (nth 3 properties) 'comint-highlight-input)
;;                                          (funcall g start end properties object)))))
;;                             (apply f args))
;;                         (apply f args))))

(provide 'dc-python)
