;;; post-init.el --- User configuration -*- lexical-binding: t; -*-

;; Add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Core
(require 'init-evil)
(require 'init-completion)

(provide 'post-init)
