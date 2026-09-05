;;; init-lang-julia.el --- Julia -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package code-cells
  :config
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key (lambda ()
                                                        "Show/hide current cell"
                                                        (interactive)
                                                        (outline-minor-mode)
                                                        (if (outline-invisible-p (line-end-position))
                                                            (outline-show-subtree)
                                                          (outline-hide-subtree)))))
    )

  (add-to-list 'code-cells-eval-region-commands '(julia-snail-mode
                                                  . julia-snail-send-code-cell)))

(use-package julia-mode
  :hook ((julia-mode . julia-snail-mode)
         (julia-mode . code-cells-mode))
  :config
  (defun /julia-mode-hook ()
    (subword-mode)
    (setq show-trailing-whitespace t))

  (add-hook 'julia-mode-hook #'/julia-mode-hook))

(use-package julia-snail
  :requires vterm
  :custom
  (julia-snail-extensions '(formatter))
  :config
  (define-key julia-snail-mode-map [remap julia-snail-send-top-level-form]
              'code-cells-eval))

(provide 'init-lang-julia)
;;; init-lang-julia.el ends here
