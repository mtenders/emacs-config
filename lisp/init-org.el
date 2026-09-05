;;; init-org.el --- Org-mode, wiki, bibliography, export -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :config
  (setq org-hide-emphasis-markers t
        org-agenda-files '("~/Nextcloud/PhD/Thermal_Photonics/org"))
  (with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-classes
                 `("lualatex-koma"
                   ;; ,(concat "[NO-DEFAULT-PACKAGES] [NO-PACKAGES]"
                   ;;          (file-to-string "./preamble.tex")
                   ;;          "[EXTRA]")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Successor to org-bullets: same bullet rendering plus modern
;; table/tag/block styling, and it's actively maintained.
(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; Automatically show latex fragments
(use-package org-fragtog)
;;   :hook (org-mode . org-fragtog-mode))

;; Wiki
;; load helm, because it's requierd for org-wiki.
(use-package helm)

(use-package org-wiki
  :defer nil
  :straight (org-wiki :host github :repo "caiorss/org-wiki")
  :init (setq org-wiki-location "~/Nextcloud/PhD/Thermal_Photonics/org"))

;;------------------------------------------------------------------------------
;; BIBLIOGRAPHY (citar; replaces ivy-bibtex now that ivy is gone)
;;------------------------------------------------------------------------------

(use-package citar
  :custom
  (org-cite-global-bibliography '("~/Nextcloud/PhD/Thermal_Photonics/Bibliography/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  ;; Use biblatex (\autocite & friends) for LaTeX export, matching the old
  ;; bibtex-completion-cite-default-command setup.
  (org-cite-export-processors '((latex biblatex)))
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/Nextcloud/PhD/Thermal_Photonics/Bibliography"))
  (citar-file-open-functions
   (list (cons t (lambda (fpath) (start-process "evince" "*evince*" "evince" fpath)))))
  :hook
  ((org-mode LaTeX-mode) . citar-capf-setup))

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode 1))

;; Access org-structure-templates with "<KEY"
(require 'org-tempo)
(add-to-list 'org-structure-template-alist
             '("sp" . "src jupyter-python :session py"))
(add-to-list 'org-structure-template-alist
             '("sj" . "src jupyter-julia :session jl"))
(add-to-list 'org-structure-template-alist
             '("sjr" . "src jupyter-julia :session jl :exports results"))
(add-to-list 'org-structure-template-alist
             '("spr" . "src jupyter-python :session py :exports results"))

;; Enable markdown export of org files
(require 'ox-md)

;;------------------------------------------------------------------------------
;; ORG-REVEAL
;;------------------------------------------------------------------------------

(use-package ox-reveal)

(provide 'init-org)
;;; init-org.el ends here
