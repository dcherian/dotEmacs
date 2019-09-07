;; elpy & jedi
;; (require 'elpy)

;; (setq elpy-modules '(;; elpy-module-highlight-indentation
;; 		     elpy-module-sane-defaults
;; 		     ;; elpy-module-company
;; 		     ;; elpy-module-flymake
;; 		     ;; elpy-module-eldoc
;; 		     elpy-module-pyvenv
;; 		     ;; elpy-module-yasnippet
;; 		     ;; elpy-module-django
;; 		     ))
;; (elpy-enable)
;; (setq elpy-get-info-from-shell nil)
;; (setq elpy-rpc-python-command "python")
;; ;; use emacs-jupyter instead
;; (define-key elpy-mode-map (kbd "C-c C-c") nil)
;; (define-key elpy-mode-map (kbd "C-c C-b") nil)
;; (define-key elpy-mode-map (kbd "C-c C-z") nil)

;; (define-key elpy-mode-map (kbd "C-<up>") 'nil)
;; (define-key elpy-mode-map (kbd "C-<down>") 'nil)

(use-package pyvenv
  :demand
  :ensure)

(use-package python
  :demand
  :bind ((:map python-mode-map
	       ("C-c C-c" . python-shell-run-region-or-line)
	       ("C-c C-b" . python-shell-send-buffer)
	       ("C-c C-v" . flycheck-list-errors)
	       ))
  :config
  (setq-default python-indent-offset 4)
  (setq python-shell-prompt-detect-failure-warning nil
	python-indent-guess-indent-offset nil
	python-shell-completion-native-enable nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (defun darya-setup ()
    (message "Setting python paths for darya.")
    (setq python-shell-interpreter "/home/deepak/miniconda3/envs/dcpy/bin/ipython")
    (setq python-shell-interpreter-args "--simple-prompt")
    (setq-default org-babel-python-command "/home/deepak/miniconda3/envs/dcpy/bin/jupyter")
    ;; (setq-default ob-ipython-command "/home/deepak/anaconda3/bin/jupyter")
    ;; (setq-default ob-ipython-kernel-extra-args 'nil)
    ;; (setq exec-path (append exec-path '("/home/deepak/miniconda3/envs/dcpy/bin/")))
    )

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
    (switch-to-buffer "*Python*")))

;; Package `anaconda-mode' seems to play well with Jupyter and adds the missing
;; eldoc and jump-to-definition functionalities without depending on the old
;; Python comint REPL. Its keybindings also don't interfere with emacs-jupyter,
;; which is nice.
(use-package anaconda-mode
  :ensure
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package python-pytest
  :ensure
  :bind (:map python-mode-map
	      ("C-c C-t" . python-pytest-popup))
  :config
  (magit-define-popup-switch 'python-pytest-popup
    ?n "parallel" "-nauto"))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 1))

(use-package beacon
  :ensure)

(use-package python-black
  :ensure
  :after python
  :bind (:map python-mode-map
              ("C-c =" . python-black-buffer)))

(use-package jupyter
  :demand t
  :after (:all org python)
  :commands (dc/jupyter-faces)
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
  :init
  ; * eldoc integration
  (defun scimax-jupyter-signature ()
    "Try to return a function signature for the thing at point."
    (when (and (eql major-mode 'org-mode)
               (string= (or (get-text-property (point) 'lang) "") "jupyter-python"))
      (save-window-excursion
     ;;; Essentially copied from (jupyter-inspect-at-point).
        (jupyter-org-with-src-block-client
         (cl-destructuring-bind (code pos)
             (jupyter-code-context 'inspect)
           (jupyter-inspect code pos nil 0)))
        (when (get-buffer "*Help*")
          (with-current-buffer "*Help*"
            (goto-char (point-min))
            (prog1
                (cond
                 ((re-search-forward "Signature:" nil t 1)
                  (buffer-substring (line-beginning-position) (line-end-position)))
                 ((re-search-forward "Docstring:" nil t 1)
                  (forward-line)
                  (buffer-substring (line-beginning-position) (line-end-position)))
                 (t
                  nil))
              ;; get rid of this so we don't accidentally show old results later
              (with-current-buffer "*Help*"
                (toggle-read-only)
                (erase-buffer))))))))

  (defun scimax-jupyter-eldoc-advice (orig-func &rest args)
    "Advice function to get eldoc signatures in blocks in org-mode."
    (or (scimax-jupyter-signature) (apply orig-func args)))

  (defun scimax-jupyter-turn-on-eldoc ()
    "Turn on eldoc signatures."
    (interactive)
    (advice-add 'org-eldoc-documentation-function :around #'scimax-jupyter-eldoc-advice))

  :config
  (add-to-list 'org-babel-load-languages '(jupyter . t) t)
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  (require 'jupyter-tramp)
  (setq jupyter-repl-echo-eval-p t
	jupyter-api-authentication-method "Token based")

  (defun dc/jupyter-faces ()
    (interactive)
    (set-face-attribute 'jupyter-repl-traceback nil
			:background nil)

    (set-face-attribute 'jupyter-repl-input-prompt nil
			:foreground "#b58900")

    (set-face-attribute 'jupyter-repl-output-prompt nil
			:foreground "#dc322f")))


;; This second `use-package' declaration only demanded after both jupyter and python
;; have been loaded. This guarantees that the org-babel-default-header-args for python
;; will exist.
(use-package ob
  :demand t
  :after (:all jupyter python)
  :bind (:map python-mode-map
	      ("s-g" . dc/org-babel-execute-named-block))
  :config
  (setq org-babel-default-header-args:jupyter-python
	'((:session . "py")
	  (:exports . "results")
	  (:kernel . "python3")
	  (:cache .   "no")
	  (:noweb . "yes")
	  (:hlines . "no")
	  (:tangle . "yes")
	  (:eval . "never-export")))
  (org-babel-jupyter-override-src-block "python")

    ;; Set better default settings for org-babel.
  ;; (setf (alist-get :async org-babel-default-header-args:jupyter-python) "yes")
  ;; (setf (alist-get :session org-babel-default-header-args:jupyter-python) "py3")

  (org-babel-lob-ingest (expand-file-name "~/org/library-of-babel.org")))

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
