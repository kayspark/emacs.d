;;; post-init.el --- User configuration -*- lexical-binding: t; -*-

;; Auto-install packages via use-package
(setq use-package-always-ensure t)

;; Add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Core
(require 'init-evil)
(require 'init-completion)
(require 'init-ui)
(require 'init-tools)
(require 'init-languages)
(require 'init-org)
(require 'init-latex-classes)

(provide 'post-init)
