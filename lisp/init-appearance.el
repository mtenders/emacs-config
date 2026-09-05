;;; init-appearance.el --- Theme, modeline, icons -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Theme

(use-package adwaita-dark-theme
  :demand t
  :config
  (if (string-equal system-type "gnu/linux")
      (load-theme 'adwaita-dark t)
    (adwaita-dark-theme-arrow-fringe-bmp-enable))
  ;; Global settings
  (set-background-color "grey19"))

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI.
  ;; "Symbols Nerd Font Mono" is the default and is recommended,
  ;; but you can use any other Nerd Font if you want.
  (nerd-icons-font-family "JetBrainsMono NF"))

(use-package doom-themes
  :demand t
  :config
  (if (string-equal system-type "darwin")
      (load-theme 'doom-one t)))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-bar-width 10))

;; Right margin for automatic linebreaks
(setq-default fill-column 80)
;; Automatic line breaks in prog-mode
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
;; Automatic line breaks with different right margin in org-mode
;; (add-hook 'org-mode-hook (lambda ()
;;                             (set-fill-column 110)))
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate
                                           c))))))

(provide 'init-appearance)
;;; init-appearance.el ends here
