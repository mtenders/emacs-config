;;; config-git.el --- Magit configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)
(require 'general)

(straight-use-package 'magit)
(general-define-key
 "C-x g" #'magit)


(provide 'config-git)
;;; config-git.el ends here
