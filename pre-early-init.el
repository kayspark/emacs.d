;;; pre-early-init.el --- Pre early-init configuration -*- lexical-binding: t; -*-
;; Frame defaults (set before GUI init to avoid flicker)
(setq default-frame-alist '((top . 1) (left . 1) (width . 115) (height . 50)
                             (font . "Iosevka SS14-18")
                             (ns-transparent-titlebar . t)))
(setq initial-frame-alist default-frame-alist)

;; Korean (Hangul) fallback: Paperlogy
(defun kp/setup-fontset ()
  "Set Paperlogy as Korean fallback and Symbols Nerd Font for icons."
  (set-fontset-font t 'hangul (font-spec :family "Paperlogy" :weight 'medium))
  (set-fontset-font t 'symbol (font-spec :family "Symbols Nerd Font Mono") nil 'prepend))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'kp/setup-fontset)
  (add-hook 'after-init-hook #'kp/setup-fontset))

;; Keep menu bar disabled, enable context menus
(setq minimal-emacs-ui-features '(context-menu))

;; Corporate proxy CA bundle for GnuTLS (Emacs ignores SSL_CERT_FILE)
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles
               (expand-file-name "~/.config/ssl/ca-bundle.pem")))
