;;; config-theme.el --- My theme configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

;;* Load doom themes
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-dracula t)
;; Correct (and improve) org-mode's native fontification.
(doom-themes-org-config)

;;* Change font depending on OS
(cond
 ((string-equal system-type "gnu/linux")
    (set-face-attribute 'default nil :family "Hack" :height 100))
 ((string-equal system-type "darwin")
    (set-face-attribute 'default nil :family "Hack" :height 120)))

;;* Load icons
(straight-use-package 'all-the-icons)

;;* Dashboard
(straight-use-package 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo
      dashboard-set-heading-icons t
      dashboard-set-file-icons t)

;;* Modeline
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)

;;* Remove unnecessary UI elements
;; Disable menu bar, tool bar, scroll bar & fringes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
;; Show battery in modeline
(display-battery-mode 1)
;; Show line numbers on the side
(global-display-line-numbers-mode)
;; Show matching parentheses
(show-paren-mode 1)

;;* Auto-fill-mode
;; Right margin for automatic linebreaks
(setq-default fill-column 80)
;; Automatic line breaks in text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;* Nyan
;; Imporant
(straight-use-package 'nyan-mode)
(nyan-mode 1)
(setq nyan-minimum-window-width 120)

(provide 'config-theme)
;;; config-theme.el ends here
