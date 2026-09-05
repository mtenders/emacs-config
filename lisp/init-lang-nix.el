;;; init-lang-nix.el --- Nix -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package nix-mode
  :mode "\\.nix\\'"
  :bind (:map nix-mode-map
              ("C-c f" . nix-format-buffer)))

(use-package nix-update
  :commands nix-update-fetch)

;; Auto-load per-project `.envrc' (e.g. `nix develop' shells) into Emacs's
;; process-environment/exec-path, requires the direnv binary.
(use-package envrc
  :demand t
  :config
  (envrc-global-mode 1))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
