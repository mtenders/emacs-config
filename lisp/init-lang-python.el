;;; init-lang-python.el --- Python -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; elpy is gone: it and eglot both tried to own completion/diagnostics for
;; python-mode at once. eglot (built into Emacs) + treesit-auto's
;; python-ts-mode cover that now.  Hooked on both python-mode and
;; python-ts-mode since treesit-auto's remap means either can be active.

;;; Code:

(use-package eglot
  :straight nil
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio"))))

;; Python code formatter
(use-package blacken
  :hook ((python-mode python-ts-mode) . blacken-mode))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
