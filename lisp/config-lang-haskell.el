;;; config-lang-haskell.el --- Haskell configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)
(require 'general)

(straight-use-package 'haskell-mode)

(general-define-key
 "<f8>" #'haskell-navigate-imports)

(setq haskell-stylish-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

(add-hook 'haskel-mode-hook #'interactive-haskell-mode)

(provide 'config-lang-haskell)
;;; config-lang-haskell.el ends here
