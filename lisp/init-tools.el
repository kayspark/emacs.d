;;; init-tools.el --- Development tools -*- lexical-binding: t; -*-

;; --- Magit ---
(use-package magit
  :defer t
  :commands (magit-status magit-log-current magit-blame)
  :config
  (setq magit-repository-directories '(("~/org" . 2) ("~/.dotfiles" . 3) ("~/workspace" . 3))
        magit-save-repository-buffers 'dontask
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; --- Ediff ---
(with-eval-after-load 'ediff
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-merge-revisions-with-ancestor t
        ediff-show-clashes-only t)
  (defvar kp/ediff-last-windows nil)
  (defun kp/store-pre-ediff-winconfig ()
    (setq kp/ediff-last-windows (current-window-configuration)))
  (defun kp/restore-pre-ediff-winconfig ()
    (set-window-configuration kp/ediff-last-windows))
  (add-hook 'ediff-before-setup-hook #'kp/store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'kp/restore-pre-ediff-winconfig))

;; --- Eglot (built-in) ---
(defun kp/eglot-ensure-safe ()
  "Start eglot only when a graphical frame exists (prevents daemon freeze)."
  (when (or (not (daemonp))
            (cl-some #'display-graphic-p (frame-list)))
    (eglot-ensure)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls"))))

;; --- Treesit-auto (auto-install grammars) ---
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))

;; --- Envrc (direnv integration) ---
(use-package envrc
  :demand t
  :config
  (envrc-global-mode 1))

;; --- Apheleia (async formatting) ---
(use-package apheleia
  :defer t
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'sqlfluff apheleia-formatters) '("sqlfluff" "fix" "--dialect" "oracle" "-"))
  (add-to-list 'apheleia-mode-alist '(sql-mode . sqlfluff)))

;; --- PDF-tools ---
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-page))

;; --- Editorconfig ---
(use-package editorconfig
  :demand t
  :config
  (editorconfig-mode 1))

;; --- Flymake (built-in, eglot uses it) ---
(global-set-key (kbd "s-q") #'flymake-show-diagnostics-buffer)

;; --- Jinx (modern spell-checker) ---
(use-package jinx
  :defer t
  :hook (text-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "en_US ko_KR"))

;; --- Vundo (visual undo) ---
(use-package vundo
  :defer t
  :commands vundo)

;; --- Ws-butler (trim trailing whitespace) ---
(use-package ws-butler
  :defer t
  :hook (prog-mode . ws-butler-mode))

;; --- Dired ---
(with-eval-after-load 'dired
  (setq dired-no-confirm t
        dired-hide-details-hide-symlink-targets t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
        dired-dwim-target t
        dired-auto-revert-buffer #'dired-directory-changed-p
        dired-make-directory-clickable t
        dired-free-space nil
        dired-mouse-drag-files t
        dired-guess-shell-alist-user
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "open")
          (".*" "open"))))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(use-package nerd-icons-dired
  :defer t
  :commands nerd-icons-dired-mode)

;; --- Isearch ---
(with-eval-after-load 'isearch
  (setq isearch-allow-scroll 'unlimited
        isearch-yank-on-move 'shift
        isearch-repeat-on-direction-change t
        isearch-wrap-pause nil
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        search-whitespace-regexp ".*?"))

;; --- Compilation ---
(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun kp/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point-max)))))
  (add-hook 'compilation-filter-hook #'kp/colorize-compilation-buffer))

;; --- Project ---
(setq project-vc-extra-root-markers '(".project"))
(setq project-key-prompt-style t)

(provide 'init-tools)
