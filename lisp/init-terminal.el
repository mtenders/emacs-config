;;; init-terminal.el --- Terminal -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package vterm  ; Needs to be loaded before julia-snail
  :config (setq vterm-shell "zsh"))

(provide 'init-terminal)
;;; init-terminal.el ends here
