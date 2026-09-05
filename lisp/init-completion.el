;;; init-completion.el --- Minibuffer completion & navigation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; vertico/orderless/marginalia/consult/embark replace the old
;; ivy/counsel/swiper stack.  This builds on Emacs's native completing-read
;; instead of reimplementing it, so it also benefits project.el, xref,
;; flymake, etc. for free.

;;; Code:

(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :bind
  (("C-s"   . #'consult-line)
   ("M-y"   . #'consult-yank-pop)
   ("C-x b" . #'consult-buffer)))

(use-package embark
  :bind
  (("C-." . #'embark-act)
   ("C-;" . #'embark-dwim)))

(use-package embark-consult
  :after (embark consult))

;; Which-key
(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

(provide 'init-completion)
;;; init-completion.el ends here
