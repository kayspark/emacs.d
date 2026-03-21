;;; init-denote-server.el --- Minimal init for denote daemon -*- lexical-binding: t; -*-
;;
;; Loaded by post-init.el when (daemonp) returns "denote".
;; Provides fast startup (~2-3s) for Raycast integration.
;; Only loads: evil + general + which-key, vertico + orderless,
;; theme + modeline, org + org-modern, denote.

;; --- Server socket (must match what Raycast expects) ---
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(add-hook 'after-init-hook
          (lambda ()
            (unless (file-directory-p server-socket-dir)
              (make-directory server-socket-dir t))
            (set-file-modes server-socket-dir #o700)))

;; --- Evil (keybindings) ---
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Skip evil-collection (heavy, 80+ mode inits) — org-mode works fine without it
;; for basic viewing/editing.

;; General.el — SPC leader
(use-package general
  :demand t
  :config
  (general-create-definer kp/leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer kp/local-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC m")

  ;; Minimal leader bindings for note viewing
  (kp/leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "find file")

    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch")
    "bd" '(kill-current-buffer :wk "kill")
    "bs" '(save-buffer :wk "save")

    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fs" '(save-buffer :wk "save")

    "n" '(:ignore t :wk "notes")
    "nf" '(denote-open-or-create :wk "find/create")

    "q" '(:ignore t :wk "quit")
    "qq" '(save-buffers-kill-terminal :wk "quit client")
    "qf" '(delete-frame :wk "delete frame")

    "w" '(:ignore t :wk "window")
    "wd" '(delete-window :wk "delete")
    "ws" '(split-window-below :wk "split below")
    "wv" '(split-window-right :wk "split right")
    "wm" '(delete-other-windows :wk "maximize")))

;; Which-key (built-in since Emacs 30)
(use-package which-key
  :ensure nil
  :demand t
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

;; macOS modifier keys
(setq mac-command-modifier       'super
      mac-option-modifier        'meta
      mac-right-command-modifier 'super
      mac-right-option-modifier  'meta)

;; --- Completion (minimal) ---
(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- Theme + Modeline ---
(use-package nepes-themes
  :vc (:url "https://github.com/kayspark/emacs-nepes" :branch "main")
  :demand t
  :config
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "nepes-themes")))
  (setq nepes-themes-mixed-fonts t
        nepes-themes-variable-pitch-ui nil
        nepes-themes-headings
        '((0 . (variable-pitch 1.6))
          (1 . (variable-pitch 1.5))
          (2 . (variable-pitch 1.4))
          (3 . (variable-pitch 1.3))
          (4 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1)))))

(defun kp/setup-theme ()
  "Load theme."
  (load-theme 'nepes-dark :no-confirm))

(defun kp/disable-menu-bar (&rest _)
  "Disable menu bar on the selected frame."
  (set-frame-parameter nil 'menu-bar-lines 0))

(menu-bar-mode -1)

(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook #'kp/setup-theme)
      (add-hook 'server-after-make-frame-hook #'kp/disable-menu-bar)
      (add-hook 'server-after-make-frame-hook #'kp/setup-fontset))
  (add-hook 'after-init-hook #'kp/setup-theme))

(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-height 30
        doom-modeline-bar-width 4
        doom-modeline-icon nil)
  (doom-modeline-mode 1))

;; Built-in padding
(setq-default left-fringe-width 20
              right-fringe-width 20)
(modify-all-frames-parameters '((internal-border-width . 15)
                                (right-divider-width . 1)))

;; --- Daemon warm-up (fast — no 3s delay since we're lightweight) ---
(when (daemonp)
  (run-with-timer 1 nil
    (lambda ()
      (unless (memq 'ns (mapcar #'framep (frame-list)))
        (condition-case err
            (let ((frame (make-frame '((window-system . ns)
                                       (visibility . nil)))))
              (with-selected-frame frame
                (kp/setup-theme)
                (kp/setup-fontset))
              (run-with-timer 1 nil #'delete-frame frame))
          (error (message "Warm-up failed: %s" err)))))))

;; --- Org ---
(with-eval-after-load 'org
  (setq org-directory "~/org"
        org-ellipsis "…"
        org-pretty-entities t
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-tags-column 0
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-block-startup t
        org-startup-align-all-tables t
        org-highlight-latex-and-related '(latex script entities))

  (setq org-todo-keywords
        '((sequence "PLANNED(p)" "TODO(t)" "PROG(g)" "REVIEW(r)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-todo-keyword-faces
        '(("PLANNED" . (:foreground "#6b7280" :weight bold))
          ("TODO"    . (:foreground "#2563eb" :weight bold))
          ("PROG"    . (:foreground "#d97706" :weight bold))
          ("REVIEW"  . (:foreground "#ea580c" :weight bold))
          ("DONE"    . (:foreground "#16a34a" :weight bold))
          ("CANCEL"  . (:foreground "#94a3b8" :weight bold))))

  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1))))

;; Org-modern — pretty rendering
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
          ("CANCEL" :inverse-video t :inherit org-todo))))

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

;; Org heading navigation (Evil keys)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    "]]" #'org-next-visible-heading
    "[[" #'org-previous-visible-heading
    "]h" #'org-next-visible-heading
    "[h" #'org-previous-visible-heading
    "]s" #'org-forward-heading-same-level
    "[s" #'org-backward-heading-same-level
    "]u" #'outline-up-heading))

(provide 'init-denote-server)
;;; init-denote-server.el ends here
