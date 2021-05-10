;;; config-lang-python.el --- Python configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

;;* Elpy
(straight-use-package 'elpy)
(add-hook 'after-init-hook #'elpy-enable)
(add-hook 'python-mode-hook #'eglot-ensure)

 ;; For elpy
  (setq elpy-rpc-python-command "python")
  ;; For interactive shell
  (setq python-shell-interpreter "python")


(provide 'config-lang-python)
;;; config-lang-python ends here
