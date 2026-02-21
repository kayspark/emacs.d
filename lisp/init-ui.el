;;; init-ui.el --- UI, theme, and editor defaults -*- lexical-binding: t; -*-

;; --- Theme ---
(use-package ef-themes
  :demand t
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui nil
        ef-themes-headings
        '((0 . (variable-pitch 1.6))
          (1 . (variable-pitch 1.5))
          (2 . (variable-pitch 1.4))
          (3 . (variable-pitch 1.3))
          (4 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1)))
        ef-themes-to-toggle '(ef-dream ef-elea-dark)))

(use-package spacious-padding
  :defer t
  :bind ("<f8>" . spacious-padding-mode)
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 1
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))
  (setq spacious-padding-subtle-mode-line nil))

;; Daemon-safe theme + spacious-padding activation
(defun kp/setup-theme-and-padding ()
  "Load theme and enable spacious-padding (GUI only)."
  (load-theme 'ef-dream :no-confirm)
  (when (display-graphic-p)
    (spacious-padding-mode 1)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'kp/setup-theme-and-padding)
  (add-hook 'after-init-hook #'kp/setup-theme-and-padding))

;; --- Fonts ---
(setq default-input-method "korean-hangul")

(defun kp/setup-fontsets ()
  "Set up hangul, emoji, and symbol fontsets for graphical frames."
  (when (display-graphic-p)
    (set-fontset-font "fontset-default" 'hangul (font-spec :family "Paperlogy"))
    (set-fontset-font "fontset-default" 'emoji (font-spec :family "Apple Color Emoji"))
    (set-fontset-font "fontset-default" 'symbol (font-spec :family "Symbols Nerd Font"))
    (let ((fp-fontset (create-fontset-from-fontset-spec
                       "-*-Iosevka Nerd Font Mono-*-*-*-*-18-*-*-*-*-*-fontset-fixedpitch")))
      (set-fontset-font fp-fontset 'hangul (font-spec :family "NanumGothicCoding"))
      (set-fontset-font fp-fontset 'emoji (font-spec :family "Apple Color Emoji"))
      (set-fontset-font fp-fontset 'symbol (font-spec :family "Symbols Nerd Font"))
      (set-face-attribute 'fixed-pitch nil :fontset fp-fontset))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'kp/setup-fontsets)
  (add-hook 'after-init-hook #'kp/setup-fontsets))

;; --- Modeline ---
(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding t
        doom-modeline-modal-modern-icon t
        doom-modeline-height 27
        doom-modeline-bar-width 3)
  (doom-modeline-mode 1))

;; --- Editor defaults ---
(setq display-line-numbers-type 'relative
      make-backup-files nil
      confirm-kill-emacs nil)

(setq-default fill-column 120
              sentence-end-double-space nil
              tab-width 2)

(setq bookmark-save-flag 1
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      delete-by-moving-to-trash t
      window-combination-resize t
      x-stretch-cursor t
      set-mark-command-repeat-pop t
      reb-re-syntax 'string
      undo-limit 80000000
      auto-save-default t
      truncate-string-ellipsis "…"
      password-cache-expiry nil
      scroll-margin 2
      insert-directory-program "/opt/local/bin/gls")

;; Deferred global modes
(add-hook 'after-init-hook
          (lambda ()
            (pixel-scroll-precision-mode 1)
            (global-visual-line-mode 1)
            (global-subword-mode 1)
            (display-time-mode 1)
            (file-name-shadow-mode 1)
            (savehist-mode 1)
            (recentf-mode 1)
            (run-with-idle-timer 2 nil #'global-auto-revert-mode 1)))

;; Persist registers and kill ring
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; --- Hl-todo ---
(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode))

;; --- Diff-hl (vc gutter) ---
(use-package diff-hl
  :defer t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; --- Pulsar (operation hints) ---
(use-package pulsar
  :defer t
  :hook (after-init . pulsar-global-mode))

;; --- Ace-window ---
(use-package ace-window
  :defer t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always t))

;; --- Tab-bar (workspaces) ---
(setq tab-bar-show 1)
(tab-bar-mode 1)

;; --- Shell ---
(setq shell-file-name "/opt/local/bin/bash"
      explicit-shell-file-name "/opt/local/bin/bash")

;; --- Auth source ---
(with-eval-after-load 'auth-source
  (setq auth-sources (nreverse auth-sources)))

;; --- Server ---
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(add-hook 'after-init-hook
          (lambda ()
            (unless (file-directory-p server-socket-dir)
              (make-directory server-socket-dir t))
            (set-file-modes server-socket-dir #o700)
            (require 'server)
            (unless (or (daemonp) (server-running-p))
              (server-start))))

(provide 'init-ui)
