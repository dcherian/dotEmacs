(use-package smartparens-config
  :ensure smartparens
  :bind (:map smartparens-mode-map
	      ("C-S-a" . sp-beginning-of-sexp)
	      ("C-S-e" . sp-end-of-sexp)

	      ("C-S-<down>" . sp-down-sexp)
	      ("C-S-<up>"   . sp-up-sexp)
	      ;; ("M-<down>" . sp-backward-down-sexp)
	      ;; ("M-<up>"   . sp-backward-up-sexp)

	      ("C-S-f" . sp-forward-sexp)
	      ("C-S-b" . sp-backward-sexp)

	      ("C-S-n" . sp-next-sexp)
	      ("C-S-p" . sp-previous-sexp)
	      ;; ("C-S-f" . sp-forward-symbol)
	      ;; ("C-S-b" . sp-backward-symbol)

	      ;; arrows based on how delimiter moves
	      ;; Shift moves left delimiter
	      ;; Alt moves right delimiter
	      ("C-M-<right>" . sp-slurp-hybrid-sexp)
	      ("C-M-<left>" . sp-forward-barf-sexp)
	      ("C-S-<left>"  . sp-backward-slurp-sexp)
	      ("C-S-<right>"  . sp-backward-barf-sexp)

	      ("C-M-t" . sp-transpose-sexp)
	      ("C-M-k" . sp-kill-sexp)
	      ("C-k"   . sp-kill-hybrid-sexp)
	      ("M-k"   . sp-backward-kill-sexp)
	      ("C-M-w" . sp-copy-sexp)
	      ("C-M-d" . delete-sexp)

	      ("M-<backspace>" . backward-kill-word)
	      ("C-<backspace>" . sp-backward-kill-word)
	      ([remap sp-backward-kill-word] . backward-kill-word)

	      ("M-[" . sp-backward-unwrap-sexp)
	      ("M-]" . sp-unwrap-sexp)

	      ("C-x C-t" . sp-transpose-hybrid-sexp)

	      ("C-x ("  . wrap-with-parens)
	      ("C-x ["  . wrap-with-brackets)
	      ("C-x {"  . wrap-with-braces)
	      ("C-x '"  . wrap-with-single-quotes)
	      ("C-x \"" . wrap-with-double-quotes)
	      ("C-x _"  . wrap-with-underscores)
	      ("C-x `"  . wrap-with-back-quotes)
	      ("C-x ~" . wrap-with-tilde))
  :config
  ;; from http://ebzzry.io/en/emacs-pairs/
  (defmacro def-pairs (pairs)
    `(progn
       ,@(cl-loop for (key . val) in pairs
		  collect
		  `(defun ,(read (concat
				  "wrap-with-"
				  (prin1-to-string key)
				  "s"))
		       (&optional arg)
		     (interactive "p")
		     (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
  	      (bracket . "[")
  	      (brace . "{")
  	      (single-quote . "'")
  	      (double-quote . "\"")
  	      (back-quote . "`")
  	      (tilde . "~")))

  (require 'smartparens-python)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(provide 'dc-parens)
