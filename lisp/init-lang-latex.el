;;; init-lang-latex.el --- LaTeX -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :hook (pdf-view-mode . pdf-view-roll-minor-mode)
  :init
  ;; pdf-loader-install only sets up the autoload that triggers real
  ;; pdf-tools-install the first time a PDF is actually opened, instead of
  ;; paying that cost on every startup regardless of whether one is.
  (pdf-loader-install))

(use-package auctex
  :init
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (setq TeX-parse-self t ; Enable parse on load.
        TeX-auto-save t ; Enable parse on save.
        ;; automatically insert braces after sub/superscript in math mode
        TeX-electric-sub-and-superscript t
        ;; just save, dont ask me before each compilation
        TeX-save-query nil)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        ;; TeX-source-correlate-mode t
        ;; TeX-source-correlate-method 'synctex
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        ;; TeX-source-correlate-start-server t
        )
  ;; (setq LaTeX-command-style
  ;;       '(("" "%(PDF)%(latex) %(file-line-error) %(extraopts) %(output-dir) -shell-escape %S%(PDFout)")))
  (setq-default TeX-engine 'luatex))

(use-package auctex-latexmk
  :after auctex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (setq TeX-command-default "LatexMk")
  :config
  (auctex-latexmk-setup))

(provide 'init-lang-latex)
;;; init-lang-latex.el ends here
