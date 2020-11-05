;;; init.el --- Main init file

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; EXWM
;; (straight-use-package 'exwm)
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

;; Lexical bindings
(setq safe-local-variable-values '((lexical-bindings . t)))

;; Use general.el to define key bindings
(straight-use-package 'general)

;; Add lisp folder to load-path for external config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load config files
(require 'config-base)
(require 'config-theme)
(require 'config-tools)
(require 'config-git)
(require 'config-org)
(require 'config-lang)
(require 'config-lang-julia)
(require 'config-lang-python)
(require 'config-lang-haskell)
(require 'config-lang-latex)

;;; init.el ends here
