
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
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(provide 'dc-ox-latex-classes)
