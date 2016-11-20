(use-package ox-nikola
  :config
  (require 'yasnippet)
  (require 'ox-publish)

  (add-to-list 'org-publish-project-alist
	       `(
		 ("website"
		  :base-directory "~/website/org/"
		  :base-extension "org"
		  :publishing-directory "~/website/pages/"
		  :publishing-function org-nikola-publish-to-rst)
		 ))

  ;; auto-insert template
  (auto-insert-mode)
  (setq auto-insert-query nil)
  (setq auto-insert-directory "~/.emacs.d/snippets/")
  (setq auto-insert 'other)

  (defun my/autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-minor-mode-on)
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert-alist
	(append
	 '((("pages/.*\\.org$" . "org-mode") . ["nikola.org" my/autoinsert-yas-expand])
	   ) auto-insert-alist)))

(provide 'dc-website)
;; (require 'dc-nikola)
