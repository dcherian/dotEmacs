(require 'ob-exp)
(require 'ox-html)
(require 'ox-publish)

(setq org-publish-project-alist 'nil)
(setq org-publish-project-alist
      '(("website" :components ("pages" "blog-static"))
	("pages"
	 :base-directory "~/website/org/"
	 :base-extension "org"
	 :recursive t
	 :html-head "<link rel=\"stylesheet\" href=\"./website.css\" type=\"text/css\" /> <link href=\"https://fonts.googleapis.com/css?family=Fira+Sans:300,400,500\" rel=\"stylesheet\">"
	 :html-postamble nil
	 :creator-info nil
	 :author nil
	 :html-doctype "xhtml5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-validation-link nil
	 :publishing-directory "~/website/publish/"
	 :publishing-function org-html-publish-to-html)

	("blog-static"
	 :base-directory "~/website/org/static/"
	 :base-extension "png\\|jpg\\|gif\\|pdf\\|mp4"
	 :publishing-directory "~/website/publish/static/"
	 :recursive t
	 :publishing-function org-publish-attachment)))

(provide 'dc-website)
