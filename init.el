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
  (let ((seperator (make-string 78 ?-))
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

;;------------------------------------------------------------------------------
;; PACKAGE MANAGMENT
;;------------------------------------------------------------------------------

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Make sure to defer as many packages as possible.
(setq use-package-always-defer t)

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

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

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
  :ensure t
  :init (doom-modeline-mode 1))

;; Right margin for automatic linebreaks
(setq-default fill-column 80)
;; Automatic line breaks in prog-mode
(add-hook 'prog-mode-hook #'turn-on-auto-fill)

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
  ; No delay in showing suggestions.
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
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

(use-package vterm
  :config (setq vterm-shell "zsh"))


;;------------------------------------------------------------------------------
;; ORG-REVEAL
;;------------------------------------------------------------------------------

(use-package ox-reveal)

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

(use-package julia-snail
  :requires vterm)

(use-package julia-mode
  :hook (julia-mode . julia-snail-mode))

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
  (setq python-shell-interpreter "python2") ;; TODO change back to python3
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "--simple-prompt -i")
  )

(use-package eglot
  :config
  (add-hook 'python-mode-hook #'eglot-ensure))

;; Python code formatter
(use-package blacken)


;;------------------------------------------------------------------------------
;; MARKDOWN
;;------------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;------------------------------------------------------------------------------
;; LATEX
;;------------------------------------------------------------------------------

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package auctex
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
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t))

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

(provide 'init)
;;; init.el ends here
