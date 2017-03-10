;;; Commentary:
;; This contains my org-mode configuration
;; - Deepak Cherian

;;; Code:
;; org-mode class for my latex style

;; for orgmk
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org/")

;; for orgmk
(unless (boundp 'dc-bindings-map)
  (defvar dc-bindings-map (make-keymap) "A keymap for custom bindings."))

(use-package org
  :load-path "/usr/local/share/emacs/site-lisp/org/"
  :bind ((:map dc-bindings-map
	       ("C-c c" . org-capture)
	       ("C-c a" . org-agenda)
	       ("C-c b" . org-iswitchb)))
  :config
  (setq org-directory "~/org")

  (define-key dc/toggle-map "h" #'org-hide-block-all)

  (setq org-startup-indented t
	org-hide-leading-stars t
	org-return-follows-link t
	org-footnote-define-inline t
	org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-ellipsis "⤵"
	org-log-done t
	org-catch-invisible-edits 'show
	org-list-allow-alphabetical t
	org-hide-emphasis-markers t
	org-image-actual-width nil
	org-export-in-background nil
	org-src-fontify-natively 1
	org-src-tab-acts-natively 1
	org-pretty-entities t
	org-pretty-entities-include-sub-superscripts t
	fill-column 90)

  (defun my-org-mode-hook ()
    (visual-fill-column-mode)
    (diminish 'org-indent-mode)
    (setq line-spacing 4))

  (add-hook 'org-mode-hook 'my-org-mode-hook)

  (set-face-attribute 'org-level-1 nil
		      :inherit 'outline-1 :height 1.25)
  (set-face-attribute 'org-level-2 nil
		      :inherit 'outline-2 :height 1.2)
  (set-face-attribute 'org-level-3 nil
		      :inherit 'outline-3 :height 1.15)
  (set-face-attribute 'org-link nil
		      :inherit 'org-link :foreground nil) ; links are only underlined
  (set-face-attribute 'org-footnote nil
		      :inherit '(font-lock-comment-face org-foreground)) ; footnotes shouldn't be highlighted
  (set-face-attribute 'org-checkbox nil
		      :inherit '(font-lock-comment-face)
		      :weight 'light
		      :box nil)
  (set-face-attribute 'org-todo nil
		      :weight 'normal)
  (set-face-attribute 'org-done nil
		      :weight 'normal)
  (set-face-attribute 'org-block nil
		      :foreground nil
		      :background "#f7f0dd")

  (setq org-file-apps
	'((auto-mode . emacs)
	  ("\\.pdf\\'" . "open %s")))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode)
    (setq org-bullets-bullet-list '("⊢" "⋮" "⋱" "•")))

  ;; my customized preamble
  (use-package ox-latex
    :defer t
    :config
    ;; org-latex-pdf-process is for org > 8.0
    ;; remove blank lines - so that I can use format statement in
    ;; #+LATEX_HEADER. set jobname so that it opens the pdf. %b command
    ;; found in ox-latex.el
    ;; (setq org-latex-pdf-process
    ;;      '("export BSTINPUTS=/usr/local/texlive/2016/texmf-dist/bibtex/bst/elsarticle/; tail -n +3 %f | sed '/./,$!d' > %f.nolines; mv %f.nolines %f; latexmk %f && (exiftool -overwrite_original -Producer=`git rev-parse HEAD` %b.pdf)"))

    (defun dc/org-latex-word-count ()
      (interactive)
      (org-latex-export-to-latex)
      (shell-command (concat "texcount "
					; "uncomment then options go here "
			     (file-name-sans-extension buffer-file-name)
			     ".tex")))
    (define-key org-mode-map "\C-cw" 'dc/org-latex-word-count)

    (setq org-latex-hyperref-template nil
	  org-latex-listings t
	  org-latex-prefer-user-labels t
	  org-latex-tables-booktabs t
	  org-latex-table-scientific-notation nil
	  org-latex-compiler-file-string nil
	  org-highlight-latex-and-related '(latex script entities))

    (setq org-latex-pdf-process
	  '("source ~/.bashrc; tail -n +3 %f | sed '/./,$!d' > %f.nolines; mv %f.nolines %f; latexmk %f; exiftool -overwrite_original -Producer=`git rev-parse HEAD` %b.pdf"))

    (add-to-list 'org-latex-classes
		 '("dcbeamer"
		   "[NO-DEFAULT-PACKAGES]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

    (add-to-list 'org-latex-classes
		 '("dcarticle"
		   "[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]
\\setsecnumdepth{subsubsection}
\\counterwithout{section}{chapter}
\\counterwithout{figure}{chapter}
\\counterwithout{table}{chapter}
"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("dcthesis"
		 "[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("ametsoc"
		 "[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\documentclass{ametsoc}
\\usepackage[english]{babel}
\\usepackage{fixltx2e}
\\usepackage[mathletters]{ucs}
\\usepackage[utf8x]{inputenx}
\\usepackage[T1]{fontenc}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{multirow}
\\usepackage{longtable}
\\usepackage{subfig}
\\usepackage{float}
\\usepackage[normalem]{ulem}
\\usepackage{etoolbox}
\\usepackage{parskip}
\\usepackage{paralist}
\\usepackage{mathtools}
\\usepackage{siunitx}
\\usepackage{xfrac}
\\usepackage{bigints}
\\usepackage[protrusion=true]{microtype}
\\sisetup{detect-all = true, separate-uncertainty = true, list-units=single, range-units=single, range-phrase = --, per-mode=reciprocal, retain-unity-mantissa=false }
\\bibpunct{(}{)}{;}{a}{}{,}
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       ))

(add-to-list 'org-latex-classes
	     '("JMR-review"
	       "[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\documentclass[12pt,titlepage]{article}
\\usepackage{endfloat}
\\usepackage{rotating}
\\usepackage{fixltx2e}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{multirow}
\\usepackage{longtable}
\\usepackage{subfig}
\\usepackage{float}
\\usepackage[normalem]{ulem}
\\usepackage{etoolbox}
\\usepackage{parskip}
\\usepackage{paralist}
\\usepackage{mathtools}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{xfrac}
\\usepackage{bigints}
\\usepackage[protrusion=true]{microtype}
\\sisetup{detect-all = true, separate-uncertainty = true, list-units=single, range-units=single, range-phrase = --, per-mode=reciprocal, retain-unity-mantissa=false }\
\\usepackage{JMR}
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       )))

(use-package org-ref
  :ensure t
  :demand t
  :bind (:map dc-bindings-map
	      ("C-c [" . org-ref-helm-insert-ref-link)
	      ("C-c ]" . org-ref-helm-insert-cite-link)
	      ("C-c \\" . org-ref-helm-insert-label-link))
  :config
  (set-face-attribute 'org-ref-cite-face nil
		      :inherit 'org-link
		      :foreground nil)
  (set-face-attribute 'org-ref-ref-face nil
		      :inherit 'org-ref-cite-face
		      :foreground nil)

  (setq org-ref-notes-directory "~/Papers/notes/"
	org-ref-bibliography-notes "~/org/papers.org"
	org-ref-default-bibliography '("~/Papers/bibtexLibrary.bib")
	org-ref-pdf-directory "~/Papers/")

  ;; make sure org-ref notes lines up with those from helm-BibTeX
  (setq org-ref-note-title-format
	"* %3a (%y): %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :DOI: %D
 :END:")

  ;; fix org-ref-open-pdf
  (defun org-ref-get-zotero-filename (key)
    "Return the pdf filename indicated by mendeley file field.
Falls back to `org-ref-get-pdf-filename' if file field does not exist.
Modified from org-ref-get-mendeley-filename.
Set BetterBiBTeX to omit title and MIME type in file field.
Argument KEY is the bibtex key."
    (let* ((results (org-ref-get-bibtex-key-and-file key))
	   (bibfile (cdr results))
	   entry)
      (with-temp-buffer
	(insert-file-contents bibfile)
	(bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	(bibtex-search-entry key nil 0)
	(setq entry (bibtex-parse-entry))
	(let ((e (org-ref-reftex-get-bib-field "file" entry)))
	  (if (> (length e) 4)
	      (let ((clean-field (replace-regexp-in-string "{\\|}\\|\\\\" "" e)))
		(let ((first-file (car (split-string clean-field ";" t))))
		  (format (concat
			   (file-name-as-directory org-ref-pdf-directory)
			   (format "%s" first-file)))))
	    (format (concat
		     (file-name-as-directory org-ref-pdf-directory)
		     "%s.pdf")
		    key))))))

  (setq org-ref-get-pdf-filename-function 'org-ref-get-zotero-filename)
  ;; for debugging
  ;; (message "file: %s" (funcall org-ref-get-pdf-filename-function "Farrar2012"))

  (use-package helm-bibtex
    :ensure t
    :defer 5
    :bind (:map dc-bindings-map
		("C-c h b" . helm-bibtex))
    :config
    (setq bibtex-completion-bibliography "~/Papers/bibtexLibrary.bib"
	  bibtex-completion-library-path "~/Papers/"
	  bibtex-completion-notes-path "~/org/papers.org"
	  bibtex-completion-pdf-field "file"))))

(provide 'dc-org)
;;; dc-org ends here
