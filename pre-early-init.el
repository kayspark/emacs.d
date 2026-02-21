;;; pre-early-init.el --- Pre early-init configuration -*- lexical-binding: t; -*-
;; Frame defaults (set before GUI init to avoid flicker)
(setq default-frame-alist '((top . 1) (left . 1) (width . 115) (height . 50)
                             (font . "Iosevka Nerd Font Mono-18")))
(setq initial-frame-alist default-frame-alist)

;; Keep menu bar disabled, enable context menus
(setq minimal-emacs-ui-features '(context-menu))
