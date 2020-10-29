;;; config-lang-python.el --- Python configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

;;* Elpy
(straight-use-package 'elpy)
(add-hook 'after-init-hook #'elpy-enable)
(add-hook 'python-mode-hook #'eglot-ensure)

;; Check system and set pyhon path
(cond
 ((string-equal system-type "gnu/linux")
  (progn
    ;; For elpy
    (setq elpy-rpc-python-command "python")
    ;; For interactive shell
    (setq python-shell-interpreter "python")))
 ((string-equal system-type "darwin")
  (progn
    (setq elpy-rpc-python-command "/usr/local/bin/python3")
    (setq python-shell-interpreter "/usr/local/bin/python3"))))

(provide 'config-lang-python)
;;; config-lang-python ends here
