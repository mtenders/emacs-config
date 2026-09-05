;;; init-lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; https://stackoverflow.com/a/36189456
(use-package impatient-mode
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp
theme=\"united\" style=\"display:none;\"> %s  </xmp><script
src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
                     (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer))))

(provide 'init-lang-markdown)
;;; init-lang-markdown.el ends here
