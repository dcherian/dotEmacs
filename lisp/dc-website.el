(require 'ob-exp)
(require 'ox-html)
(require 'ox-publish)

(setq website-foot
      "<div align=\"center\"><a rel=\"license\" class=\"cc-logo\" href=\"http://creativecommons.org/licenses/by/4.0/\"><svg class=\"cc-logo\"><use xlink:href='#icon-cc-by'></use></svg></a><br />This work is licensed under a <a rel=\"license\"  href=\"http://creativecommons.org/licenses/by/4.0/\">Creative Commons Attribution 4.0 International License</a>.</div>")

(setq website-head
      (concat
       "<link rel=\"stylesheet\" href=\"./website.css\" type=\"text/css\" />\n"
       "<link href=\"https://fonts.googleapis.com/css?family=Fira+Sans:300,400,500\" rel=\"stylesheet\">"))

(setq website-header-file "~/website/org/website-icons.html")
(defun website-header (arg)
  (with-temp-buffer
    (insert-file-contents website-header-file)
    (buffer-string)))

(setq org-publish-project-alist 'nil)
(setq org-publish-project-alist
      `(("website" :components ("pages" "posts" "blog-static"))
	("pages"
	 :base-directory "~/website/org/"
	 :base-extension "org"
	 :recursive nil
	 :html-head ,website-head
	 :html-preamble website-header
	 :html-postamble nil
	 :author "Deepak Cherian"
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-container "section"
	 :publishing-directory "~/website/publish/"
	 :publishing-function org-html-publish-to-html)

	("posts"
	 :base-directory "~/website/org/posts/"
	 :base-extension "org"
	 :recursive nil
	 :html-head ,website-head
	 :html-preamble website-header
	 :html-postamble ,website-foot
	 :author "Deepak Cherian"
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :section-numbers nil
	 :with-toc nil
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-container "section"
	 :publishing-directory "~/website/publish/posts/"
	 :publishing-function org-html-publish-to-html)

	("blog-static"
	 :base-directory "~/website/org/static/"
	 :base-extension "png\\|jpg\\|gif\\|pdf\\|mp4"
	 :publishing-directory "~/website/publish/static/"
	 :recursive t
	 :publishing-function org-publish-attachment)))

(provide 'dc-website)
