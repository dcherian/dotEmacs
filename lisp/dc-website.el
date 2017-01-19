(require 'ox-html)
(require 'ox-publish)

(setq org-publish-project-alist 'nil)
(setq org-publish-project-alist
      '(("website" :components ("posts"))
	("posts"
	 :base-directory "~/website/org/"
	 :base-extension "org"
	 :recursive t
	 :html-postamble nil
	 :html-style nil
	 :publishing-directory "~/website/"
	 :publishing-function org-html-publish-to-html)
	))

(provide 'dc-website)
