(defun dc/bay-babel-ingest ()
  (interactive)
  (org-babel-lob-ingest (expand-file-name "~/bay/scripts/lob.org")))

(dc/bay-babel-ingest)

(provide 'dc-projects)
