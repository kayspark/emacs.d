;;; init-completion.el --- Completion framework -*- lexical-binding: t; -*-

;; Vertico — vertical completion UI
(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

;; Orderless — flexible matching (space-separated components)
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia — rich annotations in minibuffer
(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

;; Corfu — in-buffer completion popup
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :config
  (global-corfu-mode 1))

;; Cape — completion-at-point extensions (dabbrev, file, etc.)
(use-package cape
  :demand t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; Consult — enhanced search/navigation commands
(use-package consult
  :defer t
  :bind (("s-a" . consult-org-agenda)
         ("s-b" . consult-buffer)
         ("s-c" . consult-flymake)
         ("s-d" . consult-dir)
         ("s-f" . consult-fd)
         ("s-g" . consult-git-grep)
         ("s-i" . consult-imenu)
         ("s-o" . consult-outline)
         ("s-r" . consult-ripgrep)
         ("s-m" . consult-mark)))

(use-package consult-dir
  :after consult)

;; Embark — contextual actions on completion candidates
(use-package embark
  :defer t
  :bind (("s-;" . embark-act)
         ("s-'" . embark-dwim)
         ("s-C" . embark-collect)
         ("s-e" . embark-export)
         ("s-E" . embark-live))
  :config
  (setq embark-quit-after-action nil
        embark-confirm-act-all nil))

(use-package embark-consult
  :after (embark consult))

;; Nerd icons for completion UIs
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-completion)
