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

;; Built-in padding (replaces spacious-padding package)
(setq-default left-fringe-width 20
              right-fringe-width 20)
(modify-all-frames-parameters '((internal-border-width . 15)
                                (right-divider-width . 1)))

(defun kp/setup-theme ()
  "Load theme."
  (load-theme 'ef-dream :no-confirm))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'kp/setup-theme)
  (add-hook 'after-init-hook #'kp/setup-theme))

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
(global-display-line-numbers-mode 1)

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
      insert-directory-program (if (executable-find "gls") "gls" "ls"))

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

;; --- Highlight TODO/FIXME/HACK keywords (built-in font-lock) ---
(defun kp/add-todo-keywords ()
  "Highlight TODO/FIXME/HACK/NOTE keywords in comments."
  (font-lock-add-keywords nil
    '(("\\<\\(TODO\\|FIXME\\|HACK\\|NOTE\\|XXX\\):" 1 'warning prepend))))
(add-hook 'prog-mode-hook #'kp/add-todo-keywords)

;; --- Diff-hl (vc gutter) ---
(use-package diff-hl
  :defer t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; --- Pulse on navigation (built-in pulse.el) ---
(defun kp/pulse-line (&rest _)
  "Briefly highlight current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (fn '(recenter-top-bottom scroll-up-command scroll-down-command
              other-window windmove-do-window-select))
  (advice-add fn :after #'kp/pulse-line))

;; --- Tab-bar (workspaces) ---
(setq tab-bar-show 1)
(tab-bar-mode 1)

;; --- Shell environment (fix PATH for GUI / daemon) ---
(use-package exec-path-from-shell
  :when (or (daemonp) (memq window-system '(mac ns x)))
  :demand t
  :config
  (exec-path-from-shell-initialize))

(let ((bash-path (or (executable-find "bash") "/bin/bash")))
  (setq shell-file-name bash-path
        explicit-shell-file-name bash-path))

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

;; --- Daemon warm-up: pre-create hidden frame to trigger GUI init ---
;; Without this, first emacsclient -c is slow (~3-5s) because macOS
;; Cocoa stack + theme + fontsets all initialize on first frame.
(when (daemonp)
  (run-with-timer 3 nil
    (lambda ()
      (let ((frame (make-frame '((visibility . nil) (width . 80) (height . 24)))))
        (run-with-timer 1 nil #'delete-frame frame)))))

(provide 'init-ui)
