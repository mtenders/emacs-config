;;; config-lang.el --- Language tools -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

;;* Company
(straight-use-package 'company)
(add-hook 'after-init-hook #'global-company-mode)
(eval-after-load 'company
  (setq company-begin-comman              #'(self-insert-command)
	company-tooltip-align-annotations t
	company-idle-delay                .1
	company-minimum-prefix-length     2
	)
  )

(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet)
        (company-abbrev company-dabbrev company-dabbrev-code)))

(straight-use-package 'company-box)
(add-hook 'company-mode-hook #'company-box-mode)

;;* Eglot
(straight-use-package 'eglot)

;;* Flycheck
(straight-use-package 'flycheck)
;; Turn on flycheck only for Elisp
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(straight-use-package 'flycheck-package)
(eval-after-load 'flycheck
  '(flycheck-package-setup))

;;* Yasnippet
(straight-use-package 'yasnippet)
(add-hook 'after-init-hook #'yas-global-mode)

;; Enable yasnippet tab completion together with company
(defun config/company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

(add-hook 'company-mode-hook (lambda ()
  (substitute-key-definition 'company-complete-common
                             'config/company-yasnippet-or-completion
                             company-active-map)))

(straight-use-package
 '(doom-snippets :type git :host github :repo "hlissner/doom-snippets"
		 :files ("*" (:exclude ".gitignore" ".editorconfig" "LICENSE" "README.md"))))

(provide 'config-lang)
;;; config-lang.el ends here
