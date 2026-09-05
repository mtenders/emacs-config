;;; init-defaults.el --- Baseline Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Set correct locale
(if (string-equal system-type "darwin")
    (setenv "LANG" "en_GB.UTF-8"))

;; On macOS/Linux, a GUI Emacs isn't started from a shell, so it doesn't
;; inherit PATH/exec-path the way a terminal-launched one would.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x pgtk))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; Steady-state garbage collection. early-init.el sets a very high threshold
;; for startup itself; gcmh takes over once Emacs is idle.
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Persist history/state across sessions.
(use-package savehist
  :straight nil
  :demand t
  :config
  (savehist-mode 1))

(use-package saveplace
  :straight nil
  :demand t
  :config
  (save-place-mode 1))

(use-package recentf
  :straight nil
  :demand t
  :config
  (setq recentf-max-saved-items 200)
  (recentf-mode 1))

;; Navigating Emacs
(show-paren-mode 1)
(electric-pair-mode 1)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Don't show warnings all the time
(setq warning-minimum-level :error)

;; Use y or n not yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ;; keyboard scroll one line at a time

(provide 'init-defaults)
;;; init-defaults.el ends here
