;;; early-init.el --- Early init file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Raise the GC threshold for the duration of startup; init-defaults.el hands
;; steady-state collection off to gcmh once packages are loaded.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el; straight.el manages packages instead.
(setq package-enable-at-startup nil)

;; Default (find-at-startup) shells out to find(1) across every cloned repo
;; on every startup to detect local modifications -- straight's own docs
;; call this out as slow. check-on-save has zero startup cost and still
;; catches changes made by editing files inside Emacs (which covers how
;; local package patches get made here); it just won't notice changes made
;; outside Emacs (e.g. a manual `git pull` in a repo) until the next save.
;; Must be set before straight's bootstrap runs.
(setq straight-check-for-modifications '(check-on-save))

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable menu bar, tool bar, scroll bar & fringes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remove decorations
;; (setq default-frame-alist
;;       (append ; Note: if there are any conflicting settings in
;;               ; ‘default-frame-alist’, it is the one that comes first that gets
;;               ; applied .
;;        '((undecorated . t)
;;          (drag-internal-border . t)
;;          (internal-border-width . 4))
;;        default-frame-alist))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; OS specific settings
(cond
 ((string-equal system-type "gnu/linux")
  ;; Set font
  (set-face-attribute 'default nil :family "CaskaydiaCove NF" :height 100))
 ((string-equal system-type "darwin")
  ;; Set font
  (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 140)
  ;; MacOS appearance
  (setq frame-resize-pixelwise t)
  (setq default-frame-alist (append '(
                                      (ns-appearance . dark)
                                      (ns-transparent-titlebar . t))
                                    default-frame-alist))
  ))

;;; early-init.el ends here
