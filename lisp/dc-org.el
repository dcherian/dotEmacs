;;; Commentary:
;; This contains my org-mode configuration
;; - Deepak Cherian

;;; Code:
;; org-mode class for my latex style

;; for orgmk
(add-to-list 'load-path "/home/deepak/.emacs.d/lisp/")
(unless (boundp 'dc-bindings-map)
  (defvar dc-bindings-map (make-keymap) "A keymap for custom bindings."))

(unless (boundp 'dc/toggle-map)
  (define-prefix-command 'dc/toggle-map)
  (define-key ctl-x-map "t" 'dc/toggle-map))

(use-package org
  :demand
  :bind (:map dc-bindings-map
	      ("C-c c" . org-capture)
	      ("C-c b" . org-iswitchb)
	      ("C-c l" . org-lint)
	      :map org-mode-map
	      ("C-c C-x l" . org-toggle-latex-fragment)
	      ("C-c C-x C-l" . org-toggle-latex-fragment)
	      ("C-c a" . org-babel-execute-to-point)
	      ("RET" . scimax/org-return)
	      ("s-j" . org-babel-next-src-block)
	      ("s-k" . org-babel-previous-src-block)
	      ("s-l" . org-edit-src-code)
	      ("s-h" . scimax-split-src-block)
	      ("s-g" . dc/org-babel-execute-named-block)
	      ("C-c C-q" . dc/org-babel-execute-current-block-qt)
	      ("C-c C-i" . dc/org-babel-execute-current-block-inline)
	      ("C-c C-v g" . dc/org-babel-execute-named-block)
	      ("C-c C-v C-g" . dc/org-babel-execute-named-block)
	      ("C-c C-p" . dc/org-babel-execute-current-block-in-shell)
	      :map inferior-python-mode-map
	      ("s-g" . dc/org-babel-execute-named-block)
	      :map python-mode-map
	      ("s-g" . dc/org-babel-execute-named-block)
	      :map org-src-mode-map
	      ("s-l" . org-edit-src-exit)
	      :map orgalist-mode-map
	      ("RET" . scimax/org-return))
  :config
  (setq org-directory "~/org")
  (require 'ox-ipynb)
  (org-babel-lob-ingest (expand-file-name "~/org/library-of-babel.org"))

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
	org-catch-invisible-edits 'smart
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

       ;; checkboxes - add new or delete empty
       ((org-at-item-checkbox-p)
	(cond
	 ;; at the end of a line.
	 ((and (eolp)
	       (not (eq 'item (car (org-element-context)))))
	  (org-insert-todo-heading nil))
	 ;; no content, delete
	 ((and (eolp) (eq 'item (car (org-element-context))))
	  (setf (buffer-substring (line-beginning-position) (point)) ""))
	 ((eq 'paragraph (car (org-element-context)))
	  (goto-char (org-element-property :end (org-element-context)))
	  (org-insert-todo-heading nil))
	 (t
	  (org-return))))

       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((org-in-item-p)
	(cond
	 ;; empty definition list
	 ((and (looking-at " ::")
	       (looking-back "- " 3))
	  (beginning-of-line)
	  (delete-region (line-beginning-position) (line-end-position)))
	 ;; empty item
	 ((and (looking-at "$")
	       (looking-back "- " 3))
	  (beginning-of-line)
	  (delete-region (line-beginning-position) (line-end-position)))
	 ;; numbered list
	 ((and (looking-at "$")
	       (looking-back "[0-9]+. " (line-beginning-position)))
	  (beginning-of-line)
	  (delete-region (line-beginning-position) (line-end-position)))
	 ;; insert new item
	 (t
	  (end-of-line)
	  (org-insert-item))))

       ;; org-heading
       ((org-at-heading-p)
	(if (not (string= "" (org-element-property :title (org-element-context))))
	    (progn
	      ;; Go to end of subtree suggested by Pablo GG on Disqus post.
	      (org-end-of-subtree)
	      (org-insert-heading-respect-content)
	      (outline-show-entry))
	  ;; The heading was empty, so we delete it
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


  (defun dc/org-babel-execute-current-block-qt ()
    (interactive)
    (setq info (org-babel-lob--src-info "mpl-qt"))
    (when info
      (org-babel-execute-src-block nil info))

    (org-babel-mark-block)
    (elpy-shell-send-region-or-buffer))

  (defun dc/org-babel-execute-current-block-in-shell ()
    (interactive)
    (org-babel-mark-block)
    (elpy-shell-send-region-or-buffer))

  (defun dc/org-babel-execute-current-block-inline ()
    (interactive)
    (setq info (org-babel-lob--src-info "mpl-inline"))
    (when info
      (org-babel-execute-src-block nil info))

    (org-babel-mark-block)
    (elpy-shell-send-region-or-buffer))

  (defun dc/org-babel-execute-named-block ()
    (interactive)
    (setq name (ivy-read "Choose code block: "
			 (append org-babel-library-of-babel
				 (org-babel-src-block-names))
			 :history 'dc/org-named-block-history
			 :require-match t
			 :sort t))
    (setq src (org-babel-find-named-block name))
    (if src
	(save-excursion
	  (goto-char src)
	  (org-babel-execute-src-block-maybe))
      (progn
	(setq info (org-babel-lob--src-info name))
	(when info
	  (org-babel-execute-src-block nil info)))))

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

(use-package org-ref
  :ensure t
  :demand
  :bind (:map dc-bindings-map
	      ("C-c [" . org-ref-ivy-insert-ref-link)
	      ("C-c ]" . org-ref-helm-insert-cite-link)
	      ("C-c \\" . org-ref-helm-insert-label-link))
  :config
  ;; (require 'org-ref-citeproc)

  (unbind-key "C-<left>" org-ref-cite-keymap)
  (unbind-key "C-<right>" org-ref-cite-keymap)

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
  :after (org-ref)
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
  :after (org)
  :config
  (org-edit-latex-mode t))

(use-package orgalist
  :ensure
  :config
  (add-hook 'git-commit-setup-hook #'orgalist-mode)
  (add-hook 'mail-mode-hook #'orgalist-mode)
  (add-hook 'message-mode-hook #'orgalist-mode))

(use-package ox-gfm
  :defer
  :ensure t)

(use-package ox-latex
  :defer
  :config
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
  (require 'dc-ox-latex-classes))

(setq org-format-latex-header
      "%&~/tools/latex/preamble-memoir
\\usepackage{fontspec}
\\usepackage{unicode-math}
\\setromanfont[Ligatures=TeX]{TeX Gyre Pagella}
\\setmathfont[math-style=ISO,bold-style=ISO]{TeX Gyre Pagella Math}
\\setmonofont{Inconsolata}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[NO-EXTRA]
\\pagenumbering{gobble}
% do not remove\n% The settings below are copied from fullpage.sty\n\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}")

;; from https://emacs.stackexchange.com/questions/30341/how-do-i-customize-the-process-that-gets-triggered-in-org-preview-latex-fragment/33172
(setq org-preview-latex-process-alist
      (quote
       ((dvipng :programs
		("lualatex" "dvipng")
		:description "dvi > png"
		:message "you need to install the programs: latex and dvipng."
		:image-input-type "dvi"
		:image-output-type "png"
		:image-size-adjust (1.0 . 1.0)
		:latex-compiler
		("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
		:image-converter
		("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))

	(dvisvgm :programs ("latex" "dvisvgm")
		 :description "dvi > svg"
		 :message
		 "you need to install the programs: latex and dvisvgm."
		 :use-xcolor t
		 :image-input-type "xdv"
		 :image-output-type "svg"
		 :image-size-adjust (1.7 . 1.5)
		 :latex-compiler
		 ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
		 :image-converter
		 ("dvisvgm %f -n -b min -c %S -o %O"))

	(imagemagick
	 :programs ("latex" "convert")
	 :description "pdf > png"
	 :message "you need to install the programs: latex and imagemagick."
	 :use-xcolor t
	 :image-input-type "pdf"
	 :image-output-type "png"
	 :image-size-adjust (1.5 1.5)
	 :latex-compiler
	 ("xelatex -pdf -interaction nonstopmode -output-directory %o %f")
	 :image-converter
	 ("convert -density 180 -trim -antialias %f -quality 100 %O")))))

(setq org-preview-latex-default-process 'imagemagick)

(use-package ov
  :ensure)

;; specify the justification you want
(plist-put org-format-latex-options :justify 'center)

(defun org-justify-fragment-overlay (beg end image imagetype)
  "Adjust the justification of a LaTeX fragment.
The justification is set by :justify in
`org-format-latex-options'. Only equations at the beginning of a
line are justified."
  (cond
   ;; Centered justification
   ((and (eq 'center (plist-get org-format-latex-options :justify))
         (= beg (line-beginning-position)))
    (let* ((img (create-image image 'imagemagick t))
           (width (car (image-size img)))
           (offset (floor (- (/ (window-text-width) 2) (/ width 2)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))
   ;; Right justification
   ((and (eq 'right (plist-get org-format-latex-options :justify))
         (= beg (line-beginning-position)))
    (let* ((img (create-image image 'imagemagick t))
           (width (car (image-display-size (overlay-get (ov-at) 'display))))
           (offset (floor (- (window-text-width) width (- (line-end-position) end)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))))

(defun org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
               (concat (buffer-substring beg end)
                       "mouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                      `(lambda ()
                                         (interactive)
                                         (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                    map)))

;; advise the function to a
(advice-add 'org--format-latex-make-overlay :after 'org-justify-fragment-overlay)
(advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)

;; That is it. If you get tired of the advice, remove it like this:
;; (advice-remove 'org--format-latex-make-overlay 'org-justify-fragment-overlay)
;; (advice-remove 'org--format-latex-make-overlay 'org-latex-fragment-tooltip)

(defun dc/org-theme ()
  (interactive)
  (set-face-attribute 'org-level-1 nil
		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'semibold
		      :height 1.25)
  (set-face-attribute 'org-level-2 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :slant 'italic
		      :weight 'medium
		      :height 1.15)
  (set-face-attribute 'org-level-3 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'semibold
		      :height 1.05)
  (set-face-attribute 'org-level-4 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'bold
		      :height 1.05)
  (set-face-attribute 'org-level-5 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'bold
		      :height 1.05)
  (set-face-attribute 'org-level-6 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'bold
		      :height 1.05)
  (set-face-attribute 'org-level-7 nil
  		      :inherit 'variable-pitch
		      :foreground nil
		      :weight 'bold
		      :height 1.05)
  (set-face-attribute 'org-link nil
		      :inherit 'fixed-pitch
		      :underline t
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
  (set-face-attribute 'org-code nil
		      :inherit 'fixed-pitch
		      :foreground "#286db2"
		      :background nil)
  (set-face-attribute 'org-todo nil
		      :weight 'normal)
  (set-face-attribute 'org-done nil
		      :weight 'normal)
  (set-face-attribute 'org-block nil
		      :inherit 'fixed-pitch
		      :foreground nil
		      :background nil)
  (set-face-attribute 'org-block-end-line nil
		      :inherit 'org-meta-line)
  (set-face-attribute 'org-target nil
		      :inherit 'fixed-pitch
		      :foreground "#586e75"
		      :background nil)
  (set-face-attribute 'org-table nil
		      :inherit 'fixed-pitch
		      :background nil)
  (set-face-attribute 'org-date nil
		      :foreground nil
		      :inherit 'org-link)
  (set-face-attribute 'org-latex-and-related nil
		      :foreground "#268bd2"
		      :family "CMU Bright"
		      :slant 'italic)
  (set-face-attribute 'org-tag nil
		      :height 0.8
		      :inherit '(font-lock-comment-face
				 org-foreground
				 fixed-pitch))
  (set-face-attribute 'org-meta-line nil
		      :inherit 'fixed-pitch
		      :height 0.85)
  (set-face-attribute 'org-document-title nil
		      :foreground nil
		      :weight 'normal)
  (set-face-attribute 'org-ref-cite-face nil
  		      :inherit 'org-link
  		      :foreground nil)
  (set-face-attribute 'org-ref-ref-face nil
  		      :inherit 'org-ref-cite-face
  		      :foreground nil)
  )

(defun my-org-mode-hook ()
  (visual-fill-column-mode)
  (visual-line-mode)
  (diminish 'org-indent-mode)
  (setq ispell-parser 'tex)
  (undo-tree-mode)
  (setq completion-at-point-functions
	'(org-completion-symbols
	  ora-cap-filesystem
	  org-completion-refs))
  (org-bullets-mode t)
  (dc/org-theme)
  (variable-pitch-mode)
  (require 'tex-mode)
  (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate #'prettify-symbols-org-latex-compose-p))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(use-package ox-reveal
  :ensure
  :config
  (setq org-reveal-root "file:///home/deepak/gits/reveal.js"))

(use-package citeproc-orgref
  :config
  (citeproc-orgref-setup))

(use-package ox-clip)

(require 'scimax-ob)

(provide 'dc-org)
;;; dc-org ends here
