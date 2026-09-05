;;; init-lang-haskell.el --- Haskell -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind ("<f8>" . #'haskell-navigate-imports)
  :config
  (setq haskell-stylish-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t))

(provide 'init-lang-haskell)
;;; init-lang-haskell.el ends here
