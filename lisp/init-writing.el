;;; init-writing.el --- Writing helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package academic-phrases)

(use-package leo
  :straight (leo :host github :repo "mtenders/emacs-leo" :branch "main")
  :config
  (setq leo-language "es"))

(provide 'init-writing)
;;; init-writing.el ends here
