;;; config-lang-latex.el --- LaTeX configuration -*- lexical-bindings:t -*-

;;; Code:

(straight-use-package 'auctex)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(setq TeX-parse-self t ; Enable parse on load.
      TeX-auto-save t ; Enable parse on save.
      )
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-mode t
      )
(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-c C-g") 'pdf-sync-forward-search)
  )

(provide 'config-lang-latex)
;;; config-lang-latex.el ends here
