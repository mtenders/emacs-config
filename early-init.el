;;; early-init.el --- Early init file

;;; Commentary:

;;; Code:

;; Disable package.el
(setq package-enable-at-startup nil)

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

;; Disable menu bar, tool bar, scroll bar & fringes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)



;; Set font
(set-face-attribute 'default nil :family "JuliaMono" :height 100)
;; other good options:
;; https://greatscott.se/fonts/alma-mono
;; "FiraCode Nerd Font"


;;; early-init.el ends here
