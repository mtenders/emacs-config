;;; init-project.el --- Project management & git -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; projectile's completing-read calls go through vertico automatically, so
;; no counsel-projectile-style bridge package is needed here.

;;; Code:

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

;;------------------------------------------------------------------------------
;; GIT
;;------------------------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . #'magit))

(provide 'init-project)
;;; init-project.el ends here
