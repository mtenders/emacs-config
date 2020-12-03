;;; config-tools.el --- Various tools

;;; Code:

;;* PDF tools
(straight-use-package 'pdf-tools)
(pdf-tools-install)

;;* Fish mode
(straight-use-package 'fish-mode)

;;* Gnuplot
(straight-use-package 'gnuplot)

;;* Emacs start up profiler
(straight-use-package 'esup)

;;* Figlet
(require 'figlet)

;;* Leo
;; HACK this package is part of melpa, but I didn't
;; manage to change just the branch to "main" from
;; the default "master".
(straight-use-package
 '(leo :host github :repo "mtenders/emacs-leo" :branch "main"))



(provide 'config-tools)
;;; config-tools.el ends here
