;;; init-keybindings.el --- Custom functions & global keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keybindings for specific packages are defined using the :bind keyword
;; from use-package, in the module where that package is configured.  Only
;; keybindings for builtin/custom functions live here.
;;
;; M-x and C-x C-f no longer need explicit rebinding to counsel-*: vertico
;; enhances the built-in execute-extended-command/find-file directly.

;;; Code:

;;------------------------------------------------------------------------------
;; FUNCTIONS
;;------------------------------------------------------------------------------

(defun me/comment-section ()
  "Print fancy comment section."
  (interactive)
  (let* ((N
          (if (boundp 'fill-column) ; Use column width if autofill-mode is used
              (- fill-column 2)
            78))
         (seperator (make-string N ?-))
         (comment-no-space (comment-string-strip comment-start 0 1)))
    (insert
     (concat "\n"
	     comment-no-space comment-no-space seperator "\n"
	     comment-no-space comment-no-space " \n"
	     comment-no-space comment-no-space seperator
	     "\n"))
    (forward-line -2)
    (end-of-line)))

(defun find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun file-to-string (file)
  "Read FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;------------------------------------------------------------------------------
;; KEYBINDINGS
;;------------------------------------------------------------------------------

;; Discovery/introspection (formerly counsel-describe-*/counsel-find-library
;; etc.); marginalia already annotates these built-ins in the minibuffer.
(global-set-key (kbd "<f1> f") #'describe-function)
(global-set-key (kbd "<f1> v") #'describe-variable)
(global-set-key (kbd "<f1> l") #'find-library)
(global-set-key (kbd "<f2> i") #'info-lookup-symbol)
(global-set-key (kbd "<f2> u") #'insert-char)
(global-set-key (kbd "<f2> j") #'set-variable)

;; Global key bindings
(global-set-key (kbd "C-c I") #'find-user-init-file)
(global-set-key (kbd "C-c ;") #'comment-or-uncomment-region)
(global-set-key (kbd "M-o")   #'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-;") #'me/comment-section)

;; Unset C-z and C-x C-z, because it crashes emacs in GUI mode
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
