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

    ;; Diagnostics
    "d" '(:ignore t :wk "diagnostics")
    "dd" '(consult-flymake :wk "flymake")
    "dl" '(flymake-show-diagnostics-buffer :wk "list")

    ;; Embark
    "a" '(embark-act :wk "embark act")
    "'" '(embark-dwim :wk "embark dwim")
    "e" '(:ignore t :wk "embark")
    "ec" '(embark-collect :wk "collect")
    "ee" '(embark-export :wk "export")
    "el" '(embark-live :wk "live")

    ;; Files
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fd" '(consult-dir :wk "directory")
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
    "oa" '(consult-org-agenda :wk "agenda")
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
    "sr" '(consult-ripgrep :wk "ripgrep")
    "sg" '(consult-git-grep :wk "git grep")
    "si" '(consult-imenu :wk "imenu")
    "so" '(consult-outline :wk "outline")
    "sm" '(consult-mark :wk "mark")
    "sF" '(consult-fd :wk "fd find")

    ;; Toggle
    "t" '(:ignore t :wk "toggle")
    "tf" '(visual-line-mode :wk "word wrap")
    "tl" '(display-line-numbers-mode :wk "line numbers")

    ;; Quit
    "q" '(:ignore t :wk "quit")
    "qq" '(save-buffers-kill-terminal :wk "quit client")
    "qQ" '(kill-emacs :wk "quit emacs")
    "qr" '(kp/restart-daemon :wk "restart daemon")
    "qf" '(delete-frame :wk "delete frame")

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

;; macOS modifier keys
(setq mac-command-modifier       'super
      mac-option-modifier        'meta
      mac-right-command-modifier 'super
      mac-right-option-modifier  'meta
      mac-pass-control-to-system nil)

;; Restart daemon: kill and re-launch emacs --daemon
(defun kp/restart-daemon ()
  "Kill running Emacs daemon and start a fresh one."
  (interactive)
  (when (daemonp)
    (let ((default-directory (expand-file-name "~/")))
      (start-process "emacs-daemon" nil "emacs" "--daemon"))
    (kill-emacs)))

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


;; --- ] / [ bracket motions: unified navigation across all modes ---
;; Heading navigation (org, markdown, outline)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    "]]" #'org-next-visible-heading
    "[[" #'org-previous-visible-heading
    "]h" #'org-next-visible-heading
    "[h" #'org-previous-visible-heading
    "]s" #'org-forward-heading-same-level
    "[s" #'org-backward-heading-same-level
    "]u" #'outline-up-heading))

(with-eval-after-load 'markdown-mode
  (evil-define-key 'normal markdown-mode-map
    "]]" #'markdown-next-heading
    "[[" #'markdown-previous-heading
    "]h" #'markdown-next-heading
    "[h" #'markdown-previous-heading
    "]u" #'markdown-up-heading))

;; Function navigation (all programming modes via beginning/end-of-defun)
;; Class navigation via treesit (covers python, java, typescript, rust, etc.)
(defun kp/treesit-class-p (node)
  "Return non-nil if NODE is a class-like definition."
  (string-match-p
   (rx (or "class_definition" "class_declaration"
           "interface_declaration" "struct_item" "impl_item"
           "enum_declaration" "enum_item"))
   (treesit-node-type node)))

(defun kp/next-class ()
  "Jump to next class definition."
  (interactive)
  (if (treesit-parser-list)
      (let ((node (treesit-search-forward
                   (treesit-node-at (point)) #'kp/treesit-class-p)))
        (if node (goto-char (treesit-node-start node))
          (user-error "No next class")))
    (user-error "No treesit parser in this buffer")))

(defun kp/prev-class ()
  "Jump to previous class definition."
  (interactive)
  (if (treesit-parser-list)
      (let ((node (treesit-search-forward
                   (treesit-node-at (point)) #'kp/treesit-class-p t)))
        (if node (goto-char (treesit-node-start node))
          (user-error "No previous class")))
    (user-error "No treesit parser in this buffer")))

(evil-define-key 'normal prog-mode-map
  "]f" #'end-of-defun
  "[f" #'beginning-of-defun
  "]c" #'kp/next-class
  "[c" #'kp/prev-class)

(provide 'init-evil)
