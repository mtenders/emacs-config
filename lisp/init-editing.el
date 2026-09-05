;;; init-editing.el --- In-buffer completion, snippets, linting -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; corfu/cape replace company; they're built on the standard
;; completion-at-point-functions machinery instead of reimplementing
;; backends, so mode/LSP-provided capfs (eglot, elisp, etc.) show up for
;; free. cape/yasnippet-capf add the generic sources company used to
;; provide (dabbrev, file, keyword, snippets).

;;; Code:

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (tab-always-indent 'complete)
  :bind (:map corfu-map
              ("C-n" . #'corfu-next)
              ("C-p" . #'corfu-previous))
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; AUCTeX's major mode is `LaTeX-mode' (capital), not the built-in
  ;; lowercase `latex-mode' -- the original config hooked the latter, which
  ;; never actually fires for .tex buffers once AUCTeX is loaded.
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode))

(use-package yasnippet-capf
  :after yasnippet
  :init
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'yasnippet-capf -90 t))))

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

;; Route eglot's flymake diagnostics through flycheck too, so every buffer
;; uses one consistent diagnostics UI regardless of whether eglot is active.
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

;;------------------------------------------------------------------------------
;; MULTIPLE CURSORS
;;------------------------------------------------------------------------------

(use-package multiple-cursors
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))

;;------------------------------------------------------------------------------
;; RAINBOW MODE (colorize color names in buffers)
;;------------------------------------------------------------------------------
(use-package rainbow-mode)

(provide 'init-editing)
;;; init-editing.el ends here
