;; elpy & jedi
(require 'elpy)

(setq elpy-modules '(;; elpy-module-highlight-indentation
		     elpy-module-sane-defaults
		     ;; elpy-module-company
		     elpy-module-flymake
		     elpy-module-eldoc
		     ;; elpy-module-pyvenv
		     ;; elpy-module-yasnippet
		     ;; elpy-module-django
		     ))
(elpy-enable)
(setq elpy-get-info-from-shell nil)
(setq elpy-rpc-python-command "python")
;; use emacs-jupyter instead
(define-key elpy-mode-map (kbd "C-c C-c") nil)
(define-key elpy-mode-map (kbd "C-c C-b") nil)
(define-key elpy-mode-map (kbd "C-c C-z") nil)

(setq eldoc-idle-delay 1)
(define-key elpy-mode-map (kbd "C-<up>") 'nil)
(define-key elpy-mode-map (kbd "C-<down>") 'nil)

(use-package beacon
  :ensure)

(use-package jupyter
  :commands (dc/jupyter-faces)
  :quelpa ((jupyter
	    :fetcher github
	    :repo "dzop/emacs-jupyter")
	   :upgrade nil)
  :bind (:map jupyter-repl-mode-map
	      ("<up>" . jupyter-repl-history-previous)
	      ("<down>" . jupyter-repl-history-next)
	      :map dc-bindings-map
	      ("C-c p" . python-shell-run-region-or-line)
	      ("C-c t" . jupyter-repl-pop-to-buffer)
	      ("C-<tab>" . org-hide-block-toggle-maybe)
	      :map org-mode-map
	      ("C-c C-v C-i" . jupyter-repl-interrupt-kernel)
	      :map inferior-python-mode-map
	      ("C-c C-v C-i" . jupyter-repl-interrupt-kernel))
  :hook ((circadian-after-load-theme jupyter-repl-mode) . dc/jupyter-faces)
  :config
  (defun dc/jupyter-faces ()
    (interactive)
    (set-face-attribute 'jupyter-repl-traceback nil
			:background nil)

    (set-face-attribute 'jupyter-repl-input-prompt nil
			:foreground "#b58900")

    (set-face-attribute 'jupyter-repl-output-prompt nil
			:foreground "#dc322f"))

  (add-hook jupyter-repl-mode-hook 'dc/jupyter-faces)

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

(setq-default python-indent-offset 4)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-indent-guess-indent-offset nil)
(setq python-shell-completion-native-enable nil)
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

(use-package ein
  :ensure
  :config
  (require 'ob-ein))


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

;; (setq org-babel-default-header-args:ipython
;;       '((:results . "none")
;; 	;;(:session . "none")
;; 	(:exports . "results")
;; 	(:cache .   "no")
;; 	(:noweb . "yes")
;; 	(:hlines . "no")
;; 	(:tangle . "yes")
;; 	(:eval . "never-export")))

;;   ;; unusuably slow
;;   ;; (add-to-list 'company-backends 'company-ob-ipython)
;;   )

(provide 'dc-python)
