;;; init-treesit.el --- Tree-sitter major modes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remaps built-in modes (python-mode, c-mode, etc.) to their -ts-mode
;; equivalents where a grammar is available.  Grammars aren't bundled with
;; Emacs; treesit-auto-install prompts to build them the first time a
;; matching file is opened.

;;; Code:

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode 1))

(provide 'init-treesit)
;;; init-treesit.el ends here
