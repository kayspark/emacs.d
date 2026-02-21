;;; post-init.el --- User configuration -*- lexical-binding: t; -*-

;; Auto-install packages via use-package
(setq use-package-always-ensure t)

;; Add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Terminal Super key decoding (WezTerm CSI u-encoding: \e[27;8;<charcode>~)
;; Maps escape sequences sent by WezTerm CMD+key bindings to Emacs s-<key>.
(unless (display-graphic-p)
  (dolist (pair '((104 . "s-h") (106 . "s-j") (107 . "s-k") (108 . "s-l")
                 (72  . "s-H") (74  . "s-J") (75  . "s-K") (76  . "s-L")
                 (48  . "s-0") (97  . "s-a") (59  . "s-;") (113 . "s-q")
                 (43  . "s-+") (45  . "s--") (62  . "s->") (60  . "s-<")
                 (110 . "s-n")))
    (define-key input-decode-map
      (format "\e[27;8;%d~" (car pair))
      (kbd (cdr pair)))))

;; Core
(require 'init-evil)
(require 'init-completion)
(require 'init-ui)
(require 'init-tools)
(require 'init-languages)
(require 'init-org)
(require 'init-latex-classes)

(provide 'post-init)
