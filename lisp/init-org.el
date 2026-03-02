;;; init-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (setq org-directory "~/org"
        org-ellipsis "…"
        org-export-allow-bind-keywords t
        org-extend-today-until 5
        org-image-actual-width '(0.9)
        org-list-allow-alphabetical t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-startup-align-all-tables t
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-tags-column 0
        org-use-property-inheritance t
        org-insert-heading-respect-content nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-edit-src-persistent-message nil
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-block-startup t
        org-hide-drawer-startup t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-hide-macro-markers t
        org-src-window-setup 'current-window
        org-highlight-latex-and-related '(latex script entities)
        org-confirm-babel-evaluate nil)

  (setq org-todo-keywords
        '((sequence "PLANNED(p)" "TODO(t)" "PROG(g)" "REVIEW(r)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-todo-keyword-faces
        '(("PLANNED" . (:foreground "#6b7280" :weight bold))
          ("TODO"    . (:foreground "#2563eb" :weight bold))
          ("PROG"    . (:foreground "#d97706" :weight bold))
          ("REVIEW"  . (:foreground "#ea580c" :weight bold))
          ("DONE"    . (:foreground "#16a34a" :weight bold))
          ("CANCEL"  . (:foreground "#94a3b8" :weight bold))))

  (setq org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)
          (?D . font-lock-doc-face)
          (?E . shadow)))

  (setq org-use-speed-commands
        (lambda ()
          (and (looking-at org-outline-regexp)
               (looking-back "^\**"))))

  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1))))

;; --- Org Modern ---
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROG" :inverse-video t :inherit org-todo)
          ("REVIEW" :inverse-video t :inherit org-todo)
          ("CANCEL" :inverse-video t :inherit org-todo))
        org-modern-footnote (cons nil (cadr org-script-display))
        org-modern-block-name
        '((t . t) ("src" "»" "«") ("example" "»–" "–«") ("export" "⏩" "⏪"))
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏") ("subtitle" . "𝙩") ("author" . "𝘼")
          ("date" . "𝘿") ("property" . "☸") ("results" . "")
          ("options" . "⌥") ("startup" . "⏻") ("bibliography" . "")
          ("include" . "⇤") ("setupfile" . "⇚")
          ("name" . "⁍") ("header" . "›") ("caption" . "☰"))))

;; --- Org Agenda ---
(with-eval-after-load 'org-agenda
  (setq org-agenda-include-diary t
        org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1))
        org-default-notes-file "~/org/inbox/notes.org"
        org-agenda-diary-file "~/org/inbox/diary.org"
        org-agenda-files '("~/org/inbox")
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; --- Org Archive ---
(with-eval-after-load 'org-archive
  (setq org-archive-location "archive.org::datetree/"))

;; --- Org Babel ---
(with-eval-after-load 'ob-ditaa
  (setq org-ditaa-jar-path "/opt/local/share/java/ditaa-0.11.0-standalone.jar"))
(with-eval-after-load 'ob-sqlite
  (setq org-babel-sqlite3-command "/opt/local/bin/sqlite3"))
(with-eval-after-load 'ob-python
  (setq org-babel-python-command "/Users/kaypark/.venvs/ds-export/bin/python3"
        org-babel-python-preamble "import matplotlib\nmatplotlib.use('Agg')"
        python-shell-interpreter "python3"))
(with-eval-after-load 'ob-js
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))
(with-eval-after-load 'org
  (setq org-babel-default-header-args:sql
        '((:results . "value table") (:colnames . "yes"))))

;; --- Jupyter ---
(with-eval-after-load 'ob-jupyter
  (setq jupyter-repl-echo-eval-p t
        jupyter-long-timeout 30
        org-babel-default-header-args:jupyter-python
        '((:kernel . "python") (:async . "yes") (:session . "py"))
        org-babel-default-header-args:jupyter-R
        '((:kernel . "R") (:async . "yes") (:session . "r"))))

;; --- Citar (bibliography) ---
(use-package citar
  :defer t
  :custom
  (citar-bibliography '("~/org/papers/bibliography.bib"
                        "~/org/papers/references.bib"
                        "~/org/papers/misc.bib"))
  (citar-library-paths '("~/org/papers/pdfs"))
  (citar-notes-paths '("~/org/papers/notes"))
  (citar-open-entry-function #'citar-open-entry-in-file))

(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode 1))

(with-eval-after-load 'oc
  (setq org-cite-global-bibliography
        '("~/org/papers/bibliography.bib"
          "~/org/papers/references.bib"
          "~/org/papers/misc.bib")))

(with-eval-after-load 'oc-csl
  (setq org-cite-csl-styles-dir "~/org/csl-styles/"
        org-cite-csl-default-style "ieee.csl"))

;; --- Org-noter ---
(use-package org-noter
  :defer t
  :commands org-noter
  :config
  (setq org-noter-notes-search-path '("~/org/notes/" "~/org/papers/notes/")
        org-noter-separate-notes-from-heading t
        org-noter-auto-save-last-location t
        org-noter-default-notes-file-names '("notes.org")
        org-noter-always-create-frame nil))

;; --- Denote ---
(use-package denote
  :defer t
  :init
  (setq denote-directory "~/org/notes/"
        denote-file-type 'org
        denote-infer-keywords t
        denote-known-keywords '("document" "dotfiles" "emacs" "macos"
                                "macports" "microsoft" "reference" "security"
                                "terminfo" "tmux" "wezterm")
        denote-prompts '(title keywords))
  :config
  (denote-rename-buffer-mode 1))

;; Paper notes silo functions (standalone, trigger autoloads)
(defvar denote-directory)  ; suppress lexical-dynamic warning for let-binding
(defun kp/denote-paper-note ()
  "Create a new denote note in the papers silo."
  (interactive)
  (let ((denote-directory "~/org/papers/notes/"))
    (call-interactively #'denote)))

(defun kp/denote-paper-find ()
  "Find notes in the papers silo."
  (interactive)
  (let ((denote-directory "~/org/papers/notes/"))
    (call-interactively #'consult-denote-find)))

(defun kp/denote-paper-grep ()
  "Grep notes in the papers silo."
  (interactive)
  (let ((denote-directory "~/org/papers/notes/"))
    (call-interactively #'consult-denote-grep)))

(use-package consult-denote
  :commands (consult-denote-find consult-denote-grep)
  :config
  (consult-denote-mode 1))

;; --- Org-download ---
(use-package org-download
  :after org
  :config
  (setq org-download-image-dir "~/org/downloads"))

;; --- Engrave-faces (LaTeX source block highlighting) ---
(use-package engrave-faces
  :defer t
  :after ox-latex
  :config
  (setq org-latex-listings 'engraved))

;; --- Mixed-pitch (built-in face remapping) ---
;; Use variable-pitch for prose, keep fixed-pitch for code/tables
(defun kp/mixed-pitch-mode ()
  "Enable variable-pitch for text with fixed-pitch overrides for code."
  (variable-pitch-mode 1)
  (dolist (face '(org-block org-block-begin-line org-block-end-line
                  org-code org-verbatim org-table org-formula
                  org-meta-line org-document-info-keyword
                  org-special-keyword org-property-value
                  org-drawer org-column org-column-title
                  line-number line-number-current-line))
    (when (facep face)
      (face-remap-add-relative face :inherit 'fixed-pitch))))
(add-hook 'org-mode-hook #'kp/mixed-pitch-mode)

;; --- Org LaTeX export ---
(with-eval-after-load 'ox-latex
  (setq org-latex-src-block-backend 'engraved
        org-latex-compiler "xelatex"
        org-latex-pdf-process
        (list (concat "LC_ALL=ko_KR.UTF-8 latexmk -f -pdf -"
                      "xelatex"
                      " -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
        org-latex-engraved-options
        '(("commandchars" . "\\\\\\{\\}")
          ("highlightcolor" . "white!95!black!80!blue")
          ("breaklines" . "true")
          ("breaksymbol" . "\\color{white!60!black}\\tiny\\ensuremath{\\hookrightarrow}"))
        org-latex-listings-options
        '(("breaklines" "true")
          ("breakatwhitespace" "true")
          ("postbreak" "\\mbox{\\textcolor{red}{$\\hookrightarrow$}\\space}"))
        org-latex-minted-options
        '(("breaklines" "true")
          ("breakanywhere" "true")))

  ;; Default packages
  (setq org-latex-default-packages-alist
        '(("" "adjustbox" nil) ("" "booktabs" nil) ("" "cancel" nil)
          ("" "capt-of" nil) ("" "caption" nil) ("" "float" nil)
          ("" "fvextra" t) ("" "graphicx" t) ("" "grffile" t)
          ("" "hanging" t) ("" "hyperref" nil) ("" "cleveref" nil)
          ("" "longtable" nil) ("hangul" "kotex" nil) ("" "pdfx" nil)
          ("" "preview" nil) ("" "rotating" nil) ("" "scrbase" nil)
          ("" "siunitx" nil) ("" "soul" nil) ("" "subcaption" nil)
          ("" "svg" nil) ("" "textcomp" t) ("" "tikz" t)
          ("" "wrapfig" nil) ("" "xcolor" t) ("normalem" "ulem" t))))

;; --- Org keybindings (leader) ---
;; Global notes prefix (not mode-specific)
(with-eval-after-load 'general
  (kp/leader-def
    "n" '(:ignore t :wk "notes")
    "nn" '(denote :wk "new note")
    "nf" '(denote-open-or-create :wk "open/create")
    "nl" '(denote-link :wk "insert link")
    "nb" '(denote-backlinks :wk "backlinks")
    "nr" '(denote-rename-file :wk "rename")
    "ns" '(consult-denote-grep :wk "grep notes")
    "nF" '(consult-denote-find :wk "find note")
    ;; Paper notes silo (~/org/papers/notes/)
    "np" '(:ignore t :wk "paper notes")
    "npn" '(kp/denote-paper-note :wk "new paper note")
    "npf" '(kp/denote-paper-find :wk "find paper note")
    "nps" '(kp/denote-paper-grep :wk "grep paper notes")))

;; Org-mode local leader bindings (deferred until org-mode loads)
(with-eval-after-load 'org
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'org-mode-map
   :prefix "SPC m"
   "" '(:ignore t :wk "local")
   "@" '(org-cite-insert :wk "insert citation")
   "v" '(org-view-output-file :wk "view export")
   "N" '(:ignore t :wk "noter")
   "Nn" '(org-noter :wk "open session")
   "Ni" '(org-noter-insert-note :wk "insert note")
   "Ns" '(org-noter-sync-current-note :wk "sync current")
   "Nj" '(org-noter-sync-next-note :wk "sync next")
   "Nk" '(org-noter-sync-prev-note :wk "sync prev")
   "Nq" '(org-noter-kill-session :wk "kill session"))

  ;; Insert
  (kp/leader-def
    "ip" '(org-download-clipboard :wk "paste image"))

  ;; Org evil keybindings
  (evil-define-key 'normal org-mode-map
    "z*" #'my/org-jump-to-heading-beginning
    "z@" #'org-mark-subtree
    "z^" #'org-sort
    "zb" #'org-backward-heading-same-level
    "zf" #'org-forward-heading-same-level
    "zn" #'org-next-visible-heading
    "zp" #'org-prev-visible-heading
    "zu" #'outline-up-heading
    "zz" #'org-refile))

;; PDF noter local leader (deferred until pdf-tools loads)
(with-eval-after-load 'pdf-tools
  (general-define-key
   :states 'normal
   :keymaps 'pdf-view-mode-map
   :prefix "SPC m"
   :non-normal-prefix "M-SPC m"
   "" '(:ignore t :wk "local")
   "n" '(org-noter :wk "open noter")))

;; Helper functions
(defun my/org-jump-to-heading-beginning ()
  "Jump to the beginning of the current heading."
  (interactive)
  (org-back-to-heading)
  (org-beginning-of-line))

(defun org-view-output-file (&optional org-file-path)
  "Visit the first output file found for the current org file."
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil)
         (extensions '("pdf" "md" "rst" "txt" "tex" "html"))
         (external '("html")))
    (dolist (ext extensions)
      (unless output-file
        (when (file-exists-p (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) external)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

;; Embark action: open PDF from cite key in denote note
(with-eval-after-load 'embark
  (defun kp/embark-open-note-pdf (file)
    "Open the PDF associated with the first cite key found in FILE."
    (interactive "fFile: ")
    (let ((cite-key nil))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "\\[cite:@\\([^]]+\\)\\]" nil t)
          (setq cite-key (match-string 1))))
      (if cite-key
          (if-let ((files (citar-get-files (list cite-key))))
              (funcall citar-file-open-function (car files))
            (message "No PDF found for cite key: %s" cite-key))
        (message "No [cite:@key] found in %s" (file-name-nondirectory file)))))
  (define-key embark-file-map (kbd "P") #'kp/embark-open-note-pdf))

(provide 'init-org)
