;;; config-tools.el --- Various tools

;;; Code:

;;* PDF tools
(straight-use-package 'pdf-tools)
(pdf-tools-install)

;;* Fish mode
(straight-use-package 'fish-mode)

;;* Gnuplot
(straight-use-package 'gnuplot)

;;* Figlet
(require 'figlet)

(provide 'config-tools)
;;; config-tools.el ends here
