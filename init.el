;;; init.el --- Main init file

;; Lexical bindings
(setq safe-local-variable-values '((lexical-bindings . t)))

;; Use general.el to define key bindings
(straight-use-package 'general)

;; Add lisp folder to load-path for external config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load config files
(require 'config-base)
(require 'config-theme)
(require 'config-tools)
(require 'config-git)
;; (require 'config-org)
(require 'config-lang)
(require 'config-lang-julia)
(require 'config-lang-python)
(require 'config-lang-haskell)
(require 'config-lang-latex)

;;; init.el ends here
