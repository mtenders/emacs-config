;;; init.el --- My init file  -*- lexical-binding: t; -*-

;;; Commentary:

;;------------------------------------------------------------------------------
;; ToDo
;;  - company
;;    - documentation window? (julia, python)
;;    - integrate into python
;;  - yas-snippet
;;    - only run in prog-mode
;;  - Font with icons
;;------------------------------------------------------------------------------

;;; Code:

;;------------------------------------------------------------------------------
;; FUNCTIONS
;;------------------------------------------------------------------------------

(defun me/comment-section ()
  "Print fancy comment section."
  (interactive)
  (let* ((N
          (if (boundp 'fill-column) ; Use column width if autofill-mode is used
              (- fill-column 2)
            78))
         (seperator (make-string N ?-))
         (comment-no-space (comment-string-strip comment-start 0 1)))
    (insert
     (concat "\n"
	     comment-no-space comment-no-space seperator "\n"
	     comment-no-space comment-no-space " \n"
	     comment-no-space comment-no-space seperator
	     "\n"))
    (forward-line -2)
    (end-of-line)))

(defun find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun file-to-string (file)
  "Read FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;------------------------------------------------------------------------------
;; PACKAGE MANAGMENT
;;------------------------------------------------------------------------------

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Make sure to defer as many packages as possible.
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; CHANGING DEFAULTS
;;------------------------------------------------------------------------------

;; Lexical bindings
(setq safe-local-variable-values '((lexical-bindings . t)))

;; Garbage collection thresholds
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Save backup files in a common place inside the user-emacs-directory.
(setq backup-directory-alist
      (list (cons "." (expand-file-name "saves" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Navigating Emacs
(use-package counsel
  :init (ivy-mode 1)
  :bind
  (("C-s" . #'swiper-isearch)
   ("M-x" . #'counsel-M-x)
   ("C-x C-f" . #'counsel-find-file)
   ("M-y" . #'counsel-yank-pop)
   ("<f1> f" . #'counsel-describe-function)
   ("<f1> v" . #'counsel-describe-variable)
   ("<f1> l" . #'counsel-find-library)
   ("<f2> i" . #'counsel-info-lookup-symbol)
   ("<f2> u" . #'counsel-unicode-char)
   ("<f2> j" . #'counsel-set-variable)
   ("C-x b" . #'ivy-switch-buffer)
   ("C-c v" . #'ivy-push-view)
   ("C-c V" . #'ivy-pop-view))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)) ; remove initial ^ input.

(use-package which-key
  :init (which-key-mode))

;; Highlight matching parentheses.
(show-paren-mode 1)
;; Automatic closing parentheses.
(electric-pair-mode 1)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Don't show warnings all the time
(setq warning-minimum-level :error)

;; Use y or n not yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ;; keyboard scroll one line at a time

;;------------------------------------------------------------------------------
;; APPEARANCE
;;------------------------------------------------------------------------------

;; Theme
(use-package doom-themes
  :demand t
  :config
  ;; Global settings
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Right margin for automatic linebreaks
(setq-default fill-column 80)
;; Automatic line breaks in prog-mode
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
;; Automatic line breaks with different right margin in org-mode
(add-hook 'org-mode-hook (lambda ()
                            (set-fill-column 110)))
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate
                                           c))))))

;;------------------------------------------------------------------------------
;; AUTOCOMPLETION
;;------------------------------------------------------------------------------

(use-package company
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'latex-mode-hook #'company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet)
        (company-abbrev company-dabbrev company-dabbrev-code)))
  ; Small delay in showing suggestions.
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-selection-wrap-around t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'latex-mode-hook #'yas-minor-mode)
  :config
  ;; Enable yasnippet tab completion together with company
  (defun config/company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-fallback-behavior nil))
      (unless (yas-expand)
        (call-interactively #'company-complete-common))))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition 'company-complete-common
                                         'config/company-yasnippet-or-completion
                                         company-active-map))))

(use-package doom-snippets
  :straight (doom-snippets :type git
                           :host github
                           :repo "hlissner/doom-snippets"
		           :files ("*"
                                   (:exclude
                                    ".gitignore"
                                    ".editorconfig"
                                    "LICENSE"
                                    "README.md"))))

;;------------------------------------------------------------------------------
;; SYNTAX CHECKING
;;------------------------------------------------------------------------------

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

;;------------------------------------------------------------------------------
;; MULTIPLE CURSORS
;;------------------------------------------------------------------------------

(use-package multiple-cursors
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))

;;------------------------------------------------------------------------------
;; TERMINAL
;;------------------------------------------------------------------------------

(use-package vterm  ; Needs to be loaded before julia-snail
  :config (setq vterm-shell "zsh"))

;;------------------------------------------------------------------------------
;; ORG-MODE
;;------------------------------------------------------------------------------


(use-package org
  :config
  (setq org-hide-emphasis-markers t
        org-agenda-files '("~/Nextcloud/PhD/Thermal_Photonics/org"))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex"))))

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               `("lualatex-koma"
                 ,(concat "[NO-DEFAULT-PACKAGES] [NO-PACKAGES]"
                          (file-to-string "./preamble.tex")
                          "[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Automatically show latex fragments
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; Wiki
;; load helm, because it's requierd for org-wiki.
(use-package helm)

(use-package org-wiki
  :defer nil
  :straight (org-wiki :host github :repo "caiorss/org-wiki")
  :init (setq org-wiki-location "~/Nextcloud/PhD/Thermal_Photonics/org"))
(require 'org-wiki) ; TODO how do you do this with use-package?

;; Reference management
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography "~/Nextcloud/PhD/Thermal_Photonics/Bibliography/bibliography.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path
        "~/Nextcloud/PhD/Thermal_Photonics/Bibliography/notes.org")
  ;; open pdf with system pdf viewer
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "evince" "*evince*" "evince" fpath)))
  (setq bibtex-completion-notes-template-one-file
        "* TODO ${title}
 :PROPERTIES:
  :Custom_ID: ${=key=}
  :AUTHOR: ${author}
  :JOURNAL: ${journaltitle}
  :YEAR: ${year}
  :VOLUME: ${volume}
  :PAGES: ${pages}
  :DOI: ${doi}
  :URL: [[${url}]]
 :END:
"))

(use-package jupyter
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))
  ;; Show images
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  ;; Switch off confirmation
  (setq org-confirm-babel-evaluate nil)

  (setq org-src-tab-acts-natively t)
  ;; Enable setting the size of inline images
  (setq org-image-actual-width nil))

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

;; Export .org to .ipynb
;; (use-package ox-ipynb
;;   :straight (ox-ipynb :host github :repo "jkitchin/ox-ipynb"))
;; (require 'ox-ipynb)


;;------------------------------------------------------------------------------
;; ORG-REVEAL
;;------------------------------------------------------------------------------

(use-package ox-reveal)


;;------------------------------------------------------------------------------
;; WRITING
;;------------------------------------------------------------------------------

(use-package academic-phrases)

;;------------------------------------------------------------------------------
;; LANGUAGES
;;-----------------------------------------------------------------------------

(use-package leo
  :straight (leo :host github :repo "mtenders/emacs-leo" :branch "main")
  :config
  (setq leo-language "es"))


;;------------------------------------------------------------------------------
;; NIX
;;------------------------------------------------------------------------------

(use-package nix-mode
  :mode "\\.nix\\'"
  :bind (:map nix-mode-map
              ("C-c f" . nix-format-buffer)))

(use-package nix-update
  :commands nix-update-fetch)

;;------------------------------------------------------------------------------
;; JULIA
;;------------------------------------------------------------------------------

(use-package code-cells
  :hook (julia-mode . code-cells-mode)
  :config

  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key (lambda ()
                                                        "Show/hide current cell"
                                                        (interactive)
                                                        (outline-minor-mode)
                                                        (if (outline-invisible-p (line-end-position))
                                                            (outline-show-subtree)
                                                          (outline-hide-subtree)))))
    )
  
  (add-to-list 'code-cells-eval-region-commands '(julia-snail-mode
                                                  . julia-snail-send-code-cell)))


(use-package julia-mode
  :hook (julia-mode . julia-snail-mode)
  :config
  (defun /julia-mode-hook ()
    (subword-mode)
    (setq show-trailing-whitespace t))

  (add-hook 'julia-mode-hook #'/julia-mode-hook))

(use-package julia-snail
  :requires vterm
  :config
  (defun julia-snail-copy-repl-region ()
    "Copy the region (requires transient-mark) to the Julia REPL and evaluate it.
This is not module-context aware."
    (interactive)
    (if (null (use-region-p))
        (user-error "No region selected")
      (let* ((block-start (region-beginning))
             (block-end (region-end))
             (text (s-trim (buffer-substring-no-properties block-start block-end))))
        (julia-snail--send-to-repl text)
        (julia-snail--flash-region (point-at-bol) (point-at-eol)))))
  
  (define-key julia-snail-mode-map [remap julia-snail-send-region]
              'julia-snail-copy-repl-region)
  (define-key julia-snail-mode-map [remap julia-snail-send-top-level-form]
              'code-cells-eval))

;;------------------------------------------------------------------------------
;; PYTHON
;;------------------------------------------------------------------------------

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'company-backends
  ;;                          'elpy-company-backend)))
  :config
  (setq python-shell-interpreter "python") ;; TODO change back to python3
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "--simple-prompt -i")
  )

(use-package eglot
  :config
  (add-hook 'python-mode-hook #'eglot-ensure))

;; Python code formatter
(use-package blacken)


;;------------------------------------------------------------------------------
;; HASKELL
;;------------------------------------------------------------------------------

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind ("<f8>" . #'haskell-navigate-imports)
  :config
  (setq haskell-stylish-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t))

;;------------------------------------------------------------------------------
;; MARKDOWN
;;------------------------------------------------------------------------------

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;------------------------------------------------------------------------------
;; LATEX
;;------------------------------------------------------------------------------

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-env-alist '(("frame" "\\begin{frame}{?}\n\n\\end{frame}\n"
  nil)))
  (setq cdlatex-command-alist
      '(("fra" "Insert frame env" "" cdlatex-environment ("frame") t nil))))

(use-package auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-c C-g" . pdf-sync-forward-search))
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


;;------------------------------------------------------------------------------
;; GIT
;;------------------------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . #'magit))


;;------------------------------------------------------------------------------
;; DIRENV
;;------------------------------------------------------------------------------

(use-package envrc)
;; "It's probably wise to do this late in your startup sequence"
(envrc-global-mode)

;;------------------------------------------------------------------------------
;; KEYBINDINGS
;;
;; Keybindings for specific packages are defind using the :bind keyword from
;; use-package.
;;
;; All other keybindings for builtin/custom functions are defined here.
;;------------------------------------------------------------------------------

;; Global key bindings
(global-set-key (kbd "C-c I") #'find-user-init-file)
(global-set-key (kbd "C-c ;") #'comment-or-uncomment-region)
(global-set-key (kbd "M-o")   #'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-;") #'me/comment-section)

;; Unset C-z and C-x C-z, because it crashes emacs in GUI mode
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'init)
;;; init.el ends here
