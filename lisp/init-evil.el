;;; init-evil.el --- Evil mode + leader keys -*- lexical-binding: t; -*-

;; Evil mode — Vim keybindings
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil  ; let evil-collection handle it
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil   ; free TAB for org-cycle in terminal
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Evil-collection — evil bindings for 80+ modes
(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

;; General.el — SPC leader key (preserves Doom muscle memory)
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

  ;; Top-level leader bindings
  (kp/leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    ":" '(eval-expression :wk "eval")
    "." '(find-file :wk "find file")
    "," '(consult-buffer :wk "switch buffer")

    ;; Buffers
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-buffer :wk "switch")
    "bn" '(next-buffer :wk "next")
    "bp" '(previous-buffer :wk "previous")
    "bd" '(kill-current-buffer :wk "kill")
    "bs" '(save-buffer :wk "save")
    "bS" '(save-some-buffers :wk "save all")

    ;; Files
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fr" '(consult-recent-file :wk "recent")
    "fs" '(save-buffer :wk "save")
    "fR" '(rename-visited-file :wk "rename")

    ;; Git
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")
    "gl" '(magit-log-current :wk "log")
    "gb" '(magit-blame :wk "blame")

    ;; Help
    "h" '(:ignore t :wk "help")
    "hf" '(describe-function :wk "function")
    "hv" '(describe-variable :wk "variable")
    "hk" '(describe-key :wk "key")

    ;; Insert
    "i" '(:ignore t :wk "insert")

    ;; Open
    "o" '(:ignore t :wk "open")
    "op" '(project-dired :wk "project dired")

    ;; Project
    "p" '(:ignore t :wk "project")
    "pp" '(project-switch-project :wk "switch")
    "pf" '(project-find-file :wk "find file")
    "ps" '(consult-ripgrep :wk "search")

    ;; Search
    "s" '(:ignore t :wk "search")
    "ss" '(consult-line :wk "line")
    "sp" '(consult-ripgrep :wk "project")
    "si" '(consult-imenu :wk "imenu")

    ;; Toggle
    "t" '(:ignore t :wk "toggle")
    "tf" '(visual-line-mode :wk "word wrap")
    "tl" '(display-line-numbers-mode :wk "line numbers")

    ;; Window
    "w" '(:ignore t :wk "window")
    "wd" '(delete-window :wk "delete")
    "ws" '(split-window-below :wk "split below")
    "wv" '(split-window-right :wk "split right")
    "wm" '(delete-other-windows :wk "maximize"))

  ;; Clipboard (system)
  (kp/leader-def
    "y" '(:ignore t :wk "clipboard"))
  (general-define-key
   :states 'visual
   :keymaps 'override
   :prefix "SPC"
   "y" '("clipboard yank" . (lambda () (interactive) (evil-use-register ?+) (call-interactively #'evil-yank))))
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC"
   "y"  '(:ignore t :wk "clipboard")
   "yp" '("paste after" . (lambda () (interactive) (evil-paste-after 1 ?+)))
   "yP" '("paste before" . (lambda () (interactive) (evil-paste-before 1 ?+)))))

;; Which-key — shows available keybindings
(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.3
        which-key-prefix-prefix "◉ "
        which-key-min-display-lines 3)
  (which-key-mode 1))

;; Terminal cursor shape: block (normal), bar (insert), underline (replace)
(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :after evil
  :config
  (evil-terminal-cursor-changer-activate))

;; macOS modifier keys
(setq mac-command-modifier       'super
      mac-option-modifier        'meta
      mac-right-command-modifier 'super
      mac-right-option-modifier  'meta
      mac-pass-control-to-system nil)

;; WezTerm CSI sequences → Super keys (terminal only)
(unless (display-graphic-p)
  (dolist (pair '(("\e[27;8;104~" . "s-h") ("\e[27;8;106~" . "s-j")
                  ("\e[27;8;107~" . "s-k") ("\e[27;8;108~" . "s-l")
                  ("\e[27;8;72~"  . "s-H") ("\e[27;8;74~"  . "s-J")
                  ("\e[27;8;75~"  . "s-K") ("\e[27;8;76~"  . "s-L")
                  ("\e[27;8;48~"  . "s-0") ("\e[27;8;97~"  . "s-a")
                  ("\e[27;8;98~"  . "s-b") ("\e[27;8;100~" . "s-d")
                  ("\e[27;8;101~" . "s-e") ("\e[27;8;103~" . "s-g")
                  ("\e[27;8;111~" . "s-o") ("\e[27;8;114~" . "s-r")
                  ("\e[27;8;59~"  . "s-;") ("\e[27;8;39~"  . "s-'")
                  ("\e[27;8;113~" . "s-q")
                  ("\e[27;8;43~"  . "s-+") ("\e[27;8;45~"  . "s--")
                  ("\e[27;8;62~"  . "s->") ("\e[27;8;60~"  . "s-<")
                  ("\e[27;8;67~"  . "s-C") ("\e[27;8;68~"  . "s-D")
                  ("\e[27;8;69~"  . "s-E") ("\e[27;8;70~"  . "s-F")
                  ("\e[27;8;77~"  . "s-M")))
    (define-key input-decode-map (car pair) (kbd (cdr pair)))))

;; Window navigation: use C-w h/j/k/l (Evil default) exclusively.
;; Super keys (s-h/j/k/l, s-H/J/K/L) freed for other purposes.
(global-set-key (kbd "s-0") 'delete-window)

;; Window zoom toggle (matches tmux C-b z, nvim C-w z)
(defvar kp/window-zoom-config nil "Saved window config for zoom toggle.")
(defun kp/window-zoom-toggle ()
  "Toggle zoom on current window (like tmux C-b z)."
  (interactive)
  (if (and kp/window-zoom-config (> (length (window-list)) 1))
      (progn (set-window-configuration kp/window-zoom-config)
             (setq kp/window-zoom-config nil))
    (if (> (length (window-list)) 1)
        (progn (setq kp/window-zoom-config (current-window-configuration))
               (delete-other-windows))
      (when kp/window-zoom-config
        (set-window-configuration kp/window-zoom-config)
        (setq kp/window-zoom-config nil)))))
(define-key evil-window-map "z" #'kp/window-zoom-toggle)

;; Window resize
(global-set-key (kbd "s-+") 'evil-window-increase-height)
(global-set-key (kbd "s--") 'evil-window-decrease-height)
(global-set-key (kbd "s->") 'evil-window-increase-width)
(global-set-key (kbd "s-<") 'evil-window-decrease-width)

;; --- ] / [ bracket motions: unified navigation across all modes ---
;; Heading navigation (org, markdown, outline)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    "]h" #'org-next-visible-heading
    "[h" #'org-previous-visible-heading
    "]s" #'org-forward-heading-same-level
    "[s" #'org-backward-heading-same-level
    "]u" #'outline-up-heading))

(with-eval-after-load 'markdown-mode
  (evil-define-key 'normal markdown-mode-map
    "]h" #'markdown-next-heading
    "[h" #'markdown-previous-heading
    "]u" #'markdown-up-heading))

;; Function navigation (all programming modes via beginning/end-of-defun)
(evil-define-key 'normal prog-mode-map
  "]f" #'end-of-defun
  "[f" #'beginning-of-defun)

(provide 'init-evil)
