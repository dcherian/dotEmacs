;;; Commentary:
;; This contains my org-mode configuration
;; - Deepak Cherian

;;; Code:
;; org-mode class for my latex style

;; for orgmk
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org/")

;; for orgmk
(unless (boundp 'dc-bindings-map)
  (defvar dc-bindings-map (make-keymap) "A keymap for custom bindings."))

(unless (boundp 'dc/toggle-map)
  (define-prefix-command 'dc/toggle-map)
  (define-key ctl-x-map "t" 'dc/toggle-map))

(define-key org-mode-map (kbd "s-j") #'org-babel-next-src-block)
(define-key org-mode-map (kbd "s-k") #'org-babel-previous-src-block)
(define-key org-mode-map (kbd "s-l") #'org-edit-src-code)
(define-key org-src-mode-map (kbd "s-l") #'org-edit-src-exit)

(use-package org
  :demand
  :bind ((:map dc-bindings-map
	       ("C-c c" . org-capture)
	       ("C-c a" . org-agenda)
	       ("C-c b" . org-iswitchb)
	       ("s-j" . org-babel-next-src-block)
	       ("s-k" . org-babel-previous-src-block)
	       ("s-l" . org-edit-src-code)
	       ("C-c l" . org-lint)
	       ;; :map org-mode-map
	       ;; ("s-j" . org-babel-next-src-block)
	       ;; ("s-k" . org-babel-previous-src-block)
	       ;; ("s-l" . org-edit-src-code)
	       ;; :map org-src-mode-map
	       ;; ("s-l" . org-edit-src-exit)
	       ))
  :config
  (setq org-directory "~/org")
  (require 'ox-ipynb)

  ;;use org mode for eml files (useful for thunderbird plugin)
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))
  (define-key dc/toggle-map "h" #'org-hide-block-all)

  (setq org-startup-indented t
	org-startup-with-inline-images "inlineimages"
	org-hide-leading-stars t
	org-return-follows-link t
	org-footnote-define-inline t
	org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-ellipsis "…"
	org-log-done t
	org-catch-invisible-edits 'show
	org-list-allow-alphabetical t
	org-hide-emphasis-markers t
	org-image-actual-width 680
	org-export-in-background nil
	org-src-fontify-natively 1
	org-src-tab-acts-natively 1
	org-src-preserve-indentation t
	org-pretty-entities t
	org-pretty-entities-include-sub-superscripts t
	org-export-dispatch-use-expert-ui t
	org-latex-image-default-width "\\textwidth"
	fill-column 90
	org-src-window-setup 'current-window
	org-export-time-stamp-file nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     (matlab . t)
     (emacs-lisp . t)
     (latex . t)
     (shell . t)))

  ;; from abo-abo
  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))

  (defun org-insert-env (env)
    (insert "\\begin{" env "}\n")
    (save-excursion
      (insert "\n\\end{" env "}")))

  (add-to-list 'org-structure-template-alist
	       '("F" "#+CAPTION: "))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     eq_u_ation _H_TML:
_h_tml    ali_g_n    _A_SCII:
^ ^       ^ ^        _C_APTION:
"
    ("s" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("C" (hot-expand "<F"))
    ("t" (hot-expand "<t"))
    ("t" (hot-expand "<t"))
    ("u" (org-insert-env "equation"))
    ("g" (org-insert-env "align"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (define-key org-mode-map "<"
    (defun org-self-insert-or-less ()
      (interactive)
      (if (looking-back "^")
	  (hydra-org-template/body)
	(self-insert-command 1))))

  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))



  (require 'org-inlinetask)
  (defun scimax/org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it.
    Use a prefix arg to get regular RET. "
    (interactive "P")
    (if ignore
	(org-return)
      (cond

       ((eq 'line-break (car (org-element-context)))
	(org-return-indent))

       ;; Open links like usual, unless point is at the end of a line.
       ;; and if at beginning of line, just press enter.
       ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
	    (bolp))
	(org-return))

       ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
       ;; Johansson!
       ((org-inlinetask-in-task-p)
	(org-return))

       ;; checkboxes too
       ((org-at-item-checkbox-p)
	(if (not (looking-back "^- \\[ \\] " (- (point) (line-beginning-position))))
	    (org-insert-todo-heading nil)
	  (setf (buffer-substring (line-beginning-position) (point)) "")
	  (org-return)))

       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((org-in-item-p)
	(if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
	    (org-insert-heading)
	  (beginning-of-line)
	  (delete-region (line-beginning-position) (line-end-position))
	  (org-return)))

       ;; org-heading
       ((org-at-heading-p)
	(if (not (string= "" (org-element-property :title (org-element-context))))
	    (progn (org-end-of-meta-data)
		   (org-insert-heading-respect-content)
		   (outline-show-entry))
	  (beginning-of-line)
	  (setf (buffer-substring
		 (line-beginning-position) (line-end-position)) "")))

       ;; tables
       ((org-at-table-p)
	(if (-any?
	     (lambda (x) (not (string= "" x)))
	     (nth
	      (- (org-table-current-dline) 1)
	      (remove 'hline (org-table-to-lisp))))
	    (org-return)
	  ;; empty row
	  (beginning-of-line)
	  (setf (buffer-substring
		 (line-beginning-position) (line-end-position)) "")
	  (org-return)))

       ;; fall-through case
       (t
	(org-return)))))

  (define-key org-mode-map (kbd "RET")
    'scimax/org-return)

  (defun ora-cap-filesystem ()
    (let (path)
      (when (setq path (ffap-string-at-point))
	(let ((compl
	       (all-completions path #'read-file-name-internal)))
	  (when compl
	    (let ((offset (ivy-completion-common-length (car compl))))
	      (list (- (point) offset) (point) compl)))))))

  (defun org-completion-refs ()
    (when (looking-back "\\\\\\(?:ref\\|label\\){\\([^\n{}]\\)*")
      (let (cands beg end)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "\\label{\\([^}]+\\)}" nil t)
	    (push (match-string-no-properties 1) cands)))
	(save-excursion
	  (up-list)
	  (setq end (1- (point)))
	  (backward-list)
	  (setq beg (1+ (point))))
	(list beg end
	      (delete (buffer-substring-no-properties beg end)
		      (nreverse cands))))))

  (defun org-completion-symbols ()
    (when (looking-back "=[a-zA-Z]+")
      (let (cands)
	(save-match-data
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
	      (cl-pushnew (match-string-no-properties 0) cands :test 'equal))
	    cands))
	(when cands
	  (list (match-beginning 0) (match-end 0) cands)))))


  ;; make prettify-symbols-mode work for latex in org files
  ;; from https://emacs.stackexchange.com/questions/33797/use-literal-greek-characters-in-latex-fragments-in-org-mode
  (defun prettify-symbols-org-latex-compose-p (start end _match)
    "Return true iff the symbol MATCH should be composed.
The symbol starts at position START and ends at position END.
This is based on prettify-symbols-default-compose-p, to be used for
applying latex prettifycations in org mode buffers."
    ;; Check that the chars should really be composed into a symbol.
    (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
			     '(?w ?_) '(?. ?\\)))
	   (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
			     '(?w ?_) '(?. ?\\))))
      (not (or
	    (and
	     ;; we don't want a $ before to stop prettification
	     ;; or is for the case the char before does not exist (beginning of buffer)
	     (/= (or (char-before start) ?$) ?$)
	     (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg))
	    (and
	     ;; we don't want a $ after to stop prettification
	     ;; or is for the case the char after does not exist (end of buffer)
	     (/= (or (char-after end) ?$) ?$)
	     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end))
	    (nth 8 (syntax-ppss))))))

  (defun dc/set-latex-pretty-org-mode ()
    (interactive)
    (prettify-symbols-mode t))

  ;; remove comments from org document for use with export hook
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun delete-org-comments (backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
				      'comment 'identity))
	  do
	  (setf (buffer-substring (org-element-property :begin comment)
				  (org-element-property :end comment))
		"")))
  ;; add to export hook
  (add-hook 'org-export-before-processing-hook 'delete-org-comments)

  (setq org-file-apps
	'((auto-mode . emacs)
	  ("\\.pdf\\'" . "mupdf %s")
	  ("\\.png\\'" . "gpicview %s")
	  ("\\.html\\'" . "firefox %s"))))

(use-package org-sticky-header
  :ensure t
  :defer
  :config
  (setq org-sticky-header-full-path 'reversed)
  (add-hook 'org-mode-hook 'org-sticky-header-mode)
  (set-face-attribute 'header-line 'nil
		      :foreground "#586e75"
		      :background "#eee8d5"
		      :underline 'unspecified
		      :height 'unspecified
		      :box 'unspecified
		      :inherit 'unspecified))

(use-package org-ref
  :ensure t
  :demand
  :bind (:map dc-bindings-map
	      ("C-c [" . org-ref-helm-insert-ref-link)
	      ("C-c ]" . org-ref-helm-insert-cite-link)
	      ("C-c \\" . org-ref-helm-insert-label-link))
  :config

  (setq org-ref-notes-directory "~/Papers/notes/"
	org-ref-bibliography-notes "~/org/papers.org"
	org-ref-default-bibliography '("~/Papers/bibtexLibrary.bib")
	org-ref-pdf-directory "~/Papers/")

  (setq org-ref-show-broken-links nil)

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
  )

(use-package helm-bibtex
  :ensure t
  :after org-ref
  :bind (:map dc-bindings-map
	      ("C-c h b" . helm-bibtex))
  :config
  (setq bibtex-completion-bibliography "~/Papers/bibtexLibrary.bib"
	bibtex-completion-library-path "~/Papers/"
	bibtex-completion-notes-path "~/org/papers.org"
	bibtex-completion-pdf-field "file"))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("⊢" "⋮" "⋱" "•")))

(use-package org-edit-latex
  :ensure t
  :config
  (org-edit-latex-mode))

(use-package ox-gfm
  :defer
  :ensure t)

;; my customized preamble
(use-package ox-latex
  :defer
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
  (defun dc/org-latex-character-count ()
    (interactive)
    (org-latex-export-to-latex)
    (shell-command (concat "texcount -char "
					; "uncomment then options go here "
			   (file-name-sans-extension buffer-file-name)
			   ".tex")))
  (define-key org-mode-map "\C-cw" 'dc/org-latex-word-count)
  (define-key org-mode-map "\C-cW" 'dc/org-latex-character-count)

  (setq org-latex-hyperref-template nil
	org-latex-listings t
	org-latex-prefer-user-labels t
	org-latex-tables-booktabs t
	org-latex-table-scientific-notation nil
	org-latex-compiler-file-string nil
	org-highlight-latex-and-related '(latex script entities))

  (add-to-list 'org-latex-listings-langs '(ipython "Python"))

  (setq org-latex-pdf-process
	'("source ~/.bashrc; sed '/./,$!d' %f > %f.nolines; mv %f.nolines %f; latexmk %f; exiftool -overwrite_original -Producer=`git rev-parse HEAD` %b.pdf"))

  (add-to-list 'org-latex-classes
	       '("dcbeamer"
		 "[NO-DEFAULT-PACKAGES]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
	       '("dcnotebook"
		 "%&~/tools/latex/preamble-memoir
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]
\\usepackage{listings}
\\usepackage{fontspec}
\\usepackage{unicode-math}
\\setromanfont[Ligatures=TeX]{TeX Gyre Pagella}
\\setmathfont[math-style=ISO,bold-style=ISO]{TeX Gyre Pagella Math}
\\setmonofont{Inconsolata}
\\setsecnumdepth{subsubsection}
\\counterwithout{section}{chapter}
\\counterwithout{figure}{chapter}
\\counterwithout{table}{chapter}"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("dcarticle"
		   "%&~/tools/latex/preamble-memoir
\\usepackage{fontspec}
\\usepackage{unicode-math}
\\setromanfont[Ligatures=TeX]{TeX Gyre Pagella}
\\setmathfont[math-style=ISO,bold-style=ISO]{TeX Gyre Pagella Math}
\\setmonofont{Inconsolata}
[NO-DEFAULT-PACKAGES]
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
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
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
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun my-org-mode-hook ()
  (visual-fill-column-mode)
  (diminish 'org-indent-mode)
  (setq line-spacing 4)
  (setq completion-at-point-functions
	'(org-completion-symbols
	  ora-cap-filesystem
	  org-completion-refs))
  (org-bullets-mode t)
  ;; org-faces
  (set-face-attribute 'org-level-1 nil
		      :inherit 'outline-1 :height 1.25)
  (set-face-attribute 'org-level-2 nil
		      :inherit 'outline-2 :height 1.2)
  (set-face-attribute 'org-level-3 nil
		      :inherit 'outline-3 :height 1.15)
  (set-face-attribute 'org-link nil
		      :inherit 'org-link
		      :foreground nil) ; links are only underlined
  ;; footnotes shouldn't be highlighted
  (set-face-attribute 'org-footnote nil
		      :foreground nil
		      :underline nil
		      :inherit '(font-lock-comment-face org-foreground))
  (set-face-attribute 'org-checkbox nil
		      :inherit '(font-lock-comment-face)
		      :background nil
		      :weight 'light
		      :box nil)
  (set-face-attribute 'org-todo nil
		      :weight 'normal)
  (set-face-attribute 'org-done nil
		      :weight 'normal)
  (set-face-attribute 'org-block nil
		      :foreground nil
		      :background "#f7f0dd")
  (set-face-attribute 'org-target nil
		      :foreground "#586e75"
		      :background nil)
  (set-face-attribute 'org-table nil
		      :family "Ubuntu Mono"
		      :background nil)
  (set-face-attribute 'org-date nil
		      :foreground nil
		      :inherit 'org-link)
  (set-face-attribute 'org-latex-and-related nil
		      :foreground "#268bd2")
  (set-face-attribute 'org-tag nil
		      :height 0.7
		      :inherit '(font-lock-comment-face org-foreground))
  (set-face-attribute 'org-ref-cite-face nil
		      :inherit 'org-link
		      :foreground nil)
  (set-face-attribute 'org-ref-ref-face nil
		      :inherit 'org-ref-cite-face
		      :foreground nil)
  (set-face-attribute 'org-meta-line nil
		      :height 0.85)


  (require 'tex-mode)
  (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate #'prettify-symbols-org-latex-compose-p)
  (dc/set-latex-pretty-org-mode))

  (add-hook 'org-mode-hook 'my-org-mode-hook)

(provide 'dc-org)
;;; dc-org ends here
