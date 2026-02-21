;;; init-latex-classes.el --- LaTeX class definitions -*- lexical-binding: t; -*-

(with-eval-after-load 'ox-latex
  (setq org-latex-classes
        '(("article"
           "\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}

\\documentclass[11pt,a4paper]{article}
\\usepackage{amssymb}
\\newcommand{\\checkboxUnchecked}{\\(\\square\\)}
\\newcommand{\\checkboxChecked}{\\(\\boxtimes\\)}
\\newcommand{\\checkboxTransitive}{\\(\\boxminus\\)}
\\usepackage[hangul]{kotex}

\\usepackage{iftex}
\\ifPDFTeX
  \\usepackage{amsmath,amssymb,amsfonts}
  \\usepackage[T1]{fontenc}
  \\usepackage[utf8]{inputenc}
  \\usepackage{lmodern}
  \\usepackage{dhucs-nanumfont}
  \\usepackage{newtxtext,newtxmath}
  \\usepackage{tgheros}
  \\usepackage{courier}
  \\renewcommand{\\rmdefault}{ptm}
  \\renewcommand{\\sfdefault}{phv}
  \\renewcommand{\\ttdefault}{pcr}
\\else
  \\usepackage{fontspec}
  \\setmainfont{Times New Roman}
  \\setsansfont{Arial}
  \\setmonofont{Courier New}

  \\setmainhangulfont[Ligatures=TeX]{Noto Serif KR}
  \\setsanshangulfont[Ligatures=TeX]{Paperlogy}
  \\setmonohangulfont[Ligatures=TeX]{NanumGothicCoding}

  \\usepackage{unicode-math}
  \\setmathfont{Latin Modern Math}
\\fi

\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}

\\usepackage{enumitem}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\usepackage[a4paper,margin=1in,left=1in]{geometry}
\\usepackage{parskip}
\\usepackage{fix-cm}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

          ("report" "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

          ("book" "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  ;; Nepes corporate template classes
  (add-to-list 'org-latex-classes
               '("nepes-article" "\\documentclass{nepes-article}\n[NO-DEFAULT-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("nepes-beamer" "\\documentclass[presentation,aspectratio=169]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("nepes-beamer-light" "\\documentclass[presentation,aspectratio=169]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))

  ;; Tables and hyperref
  (setq org-latex-tables-booktabs t
        org-latex-hyperref-template "
\\providecolor{url}{HTML}{0077bb}
\\providecolor{link}{HTML}{882255}
\\providecolor{cite}{HTML}{999933}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite
}
\\urlstyle{same} "
        org-latex-reference-command "\\cref{%s}"))

;; Fancy checkboxes for LaTeX export
(defun kp/org-export-latex-fancy-item-checkboxes (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
     (lambda (fullmatch)
       (concat "\\\\item[" (pcase (substring fullmatch 9 -3)
                             ("square"   "\\\\checkboxUnchecked")
                             ("boxminus" "\\\\checkboxTransitive")
                             ("boxtimes" "\\\\checkboxChecked")
                             (_ (substring fullmatch 9 -3))) "]"))
     text)))
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-item-functions #'kp/org-export-latex-fancy-item-checkboxes))

(provide 'init-latex-classes)
