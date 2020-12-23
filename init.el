;; -*- lexical-binding: t; -*-

;; Lexical bindings
(setq safe-local-variable-values '((lexical-bindings . t)))

;; Garbage collection thresholds
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Save backup files in a common place inside the user-emacs-directory.
(setq backup-directory-alist
      (list (cons "." (expand-file-name "saves" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;;;  Package management
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;; Theme
(use-package doom-themes
  :config
  ;; Global settings
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

;;; Auto-fill
;; Right margin for automatic linebreaks
(setq-default fill-column 80)
;; Automatic line breaks in prog-mode
(add-hook 'prog-mode-hook #'turn-on-auto-fill)


;; Init.el shortcut
(defun find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;;* General key bindings

(global-set-key (kbd "C-c I") #'find-user-init-file)
(global-set-key (kbd "C-c ;") #'comment-or-uncomment-region)
(global-set-key (kbd "M-o")   #'other-window)

(use-package counsel
  :init (ivy-mode 1)
  :bind
  (
   ("C-s" . 'swiper-isearch)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("M-y" . 'counsel-yank-pop)
   ("<f1> f" . 'counsel-describe-function)
   ("<f1> v" . 'counsel-describe-variable)
   ("<f1> l" . 'counsel-find-library)
   ("<f2> i" . 'counsel-info-lookup-symbol)
   ("<f2> u" . 'counsel-unicode-char)
   ("<f2> j" . 'counsel-set-variable)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c V" . 'ivy-pop-view)))

(use-package which-key
  :init (which-key-mode))

(use-package leo
  :straight (leo :host github :repo "mtenders/emacs-leo" :branch "main"))
