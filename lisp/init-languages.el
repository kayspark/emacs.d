;;; init-languages.el --- Language configurations -*- lexical-binding: t; -*-

;; --- Python ---
(with-eval-after-load 'python
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  (add-hook 'python-ts-mode-hook #'kp/eglot-ensure-safe))

;; --- Java (jdtls via eglot, reuses Neovim's Mason install) ---
(with-eval-after-load 'eglot
  (defun kp/jdtls-command (_interactive)
    "Return jdtls command with per-project workspace directory."
    (let* ((project (project-current))
           (root (if project (project-root project) default-directory))
           (project-name (file-name-nondirectory (directory-file-name root)))
           (workspace-dir (expand-file-name project-name "~/.cache/emacs-jdtls")))
      `(,(expand-file-name "~/.local/share/nvim/mason/bin/jdtls")
        "--java-executable" ,(expand-file-name "~/.config/bin/jdtls-java")
        "--jvm-arg=-Xmx2G"
        "-data" ,workspace-dir)))

  (add-to-list 'eglot-server-programs '(java-mode . kp/jdtls-command))
  (add-to-list 'eglot-server-programs '(java-ts-mode . kp/jdtls-command)))

;; Per-buffer workspace settings for jdtls
(defun kp/java-eglot-workspace-config (server)
  "Return jdtls workspace configuration for SERVER."
  (ignore server)
  `(:java
    (:import
     (:gradle
      (:java
       (:home "/Library/Java/JavaVirtualMachines/openjdk8-temurin/Contents/Home")))
     :configuration
     (:runtimes
      [(:name "JavaSE-1.8"
        :path "/Library/Java/JavaVirtualMachines/openjdk8-temurin/Contents/Home"
        :default t)
       (:name "JavaSE-11"
        :path "/opt/local/Library/Java/JavaVirtualMachines/openjdk11-temurin/Contents/Home")
       (:name "JavaSE-21"
        :path "/opt/local/Library/Java/JavaVirtualMachines/jdk-21-eclipse-temurin.jdk/Contents/Home")]))))

(setq-default eglot-workspace-configuration #'kp/java-eglot-workspace-config)

(add-hook 'java-ts-mode-hook #'kp/eglot-ensure-safe)

;; --- LaTeX ---
(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . kp/eglot-ensure-safe)
  :hook (LaTeX-mode . turn-on-reftex)
  :hook (LaTeX-mode . auto-fill-mode)
  :hook (LaTeX-mode . TeX-PDF-mode)
  :config
  (setq-default TeX-engine 'xetex
                TeX-master t)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-save-query nil
        TeX-show-compilation t
        TeX-command-extra-options "-shell-escape"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq reftex-default-bibliography '("~/org/papers/bibliography.bib")
        reftex-plug-into-AUCTeX t
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)
  (setq TeX-view-program-list
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")
          ("Preview" "/usr/bin/open -a Preview.app %o")))
  (setq TeX-view-program-selection
        '((output-pdf "Preview") (output-dvi "Skim") (output-html "open")))
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %(extraopts) %t" TeX-run-TeX nil t))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package cdlatex
  :defer t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  (setq cdlatex-env-alist
        '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (setq cdlatex-math-symbol-alist
        '((?_ ("\\downarrow"  ""           "\\inf"))
          (?2 ("^2"           "\\sqrt{?}"     ""     ))
          (?3 ("^3"           "\\sqrt[3]{?}"  ""     ))
          (?^ ("\\uparrow"    ""           "\\sup"))
          (?k ("\\kappa"      ""           "\\ker"))
          (?m ("\\mu"         ""           "\\lim"))
          (?c (""             "\\circ"     "\\cos"))
          (?d ("\\delta"      "\\partial"  "\\dim"))
          (?D ("\\Delta"      "\\nabla"    "\\deg"))
          (?F ("\\Phi"))
          (?. ("\\cdot" "\\dots"))
          (?: ("\\vdots" "\\ddots"))
          (?* ("\\times" "\\star" "\\ast")))
        cdlatex-math-modify-alist
        '((?B "\\mathbb"        nil          t    nil  nil)
          (?a "\\abs"           nil          t    nil  nil))))

(use-package laas
  :defer t
  :hook (LaTeX-mode . laas-mode))

;; --- SQL ---
(with-eval-after-load 'sql
  (setq sql-sqlite-program "/opt/local/bin/sqlite3"
        sql-oracle-program "sql"
        sql-sqlite-options '("-header" "-column" "-interactive")
        sql-use-indent-support t))
(add-hook 'sql-mode-hook #'kp/eglot-ensure-safe)

;; --- Markdown ---
(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command "multimarkdown"
        markdown-enable-math t
        markdown-enable-highlighting-syntax t
        markdown-enable-wiki-links t)
  (setq markdown-code-lang-modes
        (append '(("python" . python-mode)
                  ("sql" . sql-mode))
                markdown-code-lang-modes)))

;; --- Rust (on-demand) ---
(use-package rustic
  :defer t
  :config
  (setq rustic-format-on-save t
        rustic-indent-offset 2)
  (add-hook 'rustic-mode-hook #'kp/eglot-ensure-safe))

;; --- R (on-demand) ---
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(R-mode . ("R" "--slave" "-e" "languageserver::run()"))))
(add-hook 'R-mode-hook #'kp/eglot-ensure-safe)

;; --- C/C++ (on-demand) ---
(with-eval-after-load 'cc-mode
  (setq c-basic-offset 2))

;; --- Emacs Lisp ---
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

(provide 'init-languages)
