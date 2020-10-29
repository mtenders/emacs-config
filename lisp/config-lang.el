;;; config-lang.el --- Programming language tools -*- lexical-bindings:t -*-

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

(straight-use-package
 '(doom-snippets :type git :host github :repo "hlissner/doom-snippets"))

(provide 'config-lang)
;;; config-lang.el ends here
