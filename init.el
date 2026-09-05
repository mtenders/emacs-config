;;; init.el --- My init file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Entry point only.  Actual configuration lives in lisp/init-*.el, loaded
;; below in dependency order (completion UI before things that hook into it,
;; treesit before the language modes that rely on it, etc.).

;;; Code:

;;------------------------------------------------------------------------------
;; PACKAGE MANAGEMENT
;;------------------------------------------------------------------------------

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Make sure to defer as many packages as possible.
(setq use-package-always-defer t)

;; Keep var/cache/backup files out of the top-level config directory. Must be
;; configured before other packages so it can redirect their default paths.
(use-package no-littering
  :demand t
  :config
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory))
  (no-littering-theme-backups))

;; Keep customize-generated variables out of version control.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;------------------------------------------------------------------------------
;; LOAD MODULES
;;------------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(dolist (module '(init-defaults
                   init-completion
                   init-appearance
                   init-treesit
                   init-editing
                   init-project
                   init-org
                   init-lang-python
                   init-lang-julia
                   init-lang-haskell
                   init-lang-nix
                   init-lang-latex
                   init-lang-markdown
                   init-writing
                   init-terminal
                   init-keybindings))
  (require module))

(provide 'init)
;;; init.el ends here
