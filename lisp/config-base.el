;;; config-base.el --- Configuration of base behaviour -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)
(require 'general)

;;* Save backup files in a common place inside the user-emacs-directory.
(setq backup-directory-alist
      (list (cons "." (expand-file-name "saves" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;* Add path for programs installed with brew (darwin)
(when (string-equal system-type "darwin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
  (setq exec-path (append exec-path '("/usr/local/bin/")))
  )

;;* Init.el shortcut
(defun find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;;* General key bindings
(general-define-key
 "C-c I" #'find-user-init-file
 "C-c ;" #'comment-or-uncomment-region
 "M-o"   #'other-window)

;;* Crux keybinding replacements
(straight-use-package 'crux)
(general-define-key
 [remap move-beginning-of-line] #'crux-move-beginning-of-line
 "C-x 4 t"                      #'crux-transpose-windows
 "C-k"                          #'crux-smart-kill-line)

;;* Helm
(straight-use-package 'helm)
(setq-default helm-M-x-fuzzy-match t)
;; Helm keybinding replacements
(general-define-key
 "M-x" #'helm-M-x
 "C-x C-f" #'helm-find-files)

;;* Which-key
(straight-use-package 'which-key)
(which-key-mode)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'config-base)
;;; config-base.el ends here
