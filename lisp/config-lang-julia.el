;;; config-lang-julia.el --- Julia configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

;;* Julia mode
(straight-use-package 'julia-mode)

;;* Julia repl
(straight-use-package 'julia-repl)
(add-hook 'julia-mode-hook #'julia-repl-mode)

;;* Eglot-jl
(straight-use-package 'eglot-jl)

(defun config/julia-eglot ()
  "Start eglot in `julia-mode'."
  (eglot-ensure)
  (when (< eglot-connect-timeout 60)
    (setq-local eglot-connect-timeout 60)) ;Set higher timeout for SymbolServer.jl
  (eglot-jl-init) ;Setup julia language server.
  )

(add-hook 'julia-mode-hook #'config/julia-eglot)

(provide 'config-lang-julia)
;;; config-lang-julia.el ends here
