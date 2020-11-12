;;; config-org.el --- Org configuration -*- lexical-bindings:t -*-

;;; Code:
(require 'straight)

(straight-use-package 'org)
(straight-use-package 'org-plus-contrib)

;;* Jupyter
(straight-use-package 'jupyter :no-native-compile t)

;;* Julia babel from org-plus-contrib
(cond
 ((string-equal system-type "gnu/linux")
  (setq inferior-julia-program-name "/usr/bin/julia"))
 ((string-equal system-type "darwin")
  (setq inferior-julia-program-name "/usr/local/bin/julia")))

(require 'ob-julia)

;;* babel config
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (julia . t)
   (latex . t)
   (gnuplot . t)
   (jupyter . t)
   ))
;; Show images
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
;; Switch off confirmation
(setq org-confirm-babel-evaluate nil)

(setq org-src-tab-acts-natively t)

;; Enable setting the size of inline images
(setq org-image-actual-width nil)

(add-to-list 'org-structure-template-alist '("sp" . "src jupyter-python :session py"))
(add-to-list 'org-structure-template-alist '("sb" . "src bash :results output"))
(add-to-list 'org-structure-template-alist '("sj" . "src jupyter-julia :session jl"))
(add-to-list 'org-structure-template-alist '("sjr" . "src jupyter-julia :session jl :exports results"))

;;* org-bullets
(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;* org-tempo
;; Use <X shortcuts for org-structure-alist
(require 'org-tempo)

;;* ox-ipynb (.org -> .ipynb)
(straight-use-package
 '(ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb"))

;;* htmlize
(straight-use-package 'htmlize)

(provide 'config-org)
;;; config-org.el ends here
