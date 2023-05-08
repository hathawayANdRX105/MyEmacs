;;; package --- summary
;;; commentary:
;;; init-terminal contains package configuration used for browsing projects or codes.

;;; Code:

(use-package vterm
  :hook
  (vterm-mode . (lambda () (setq-local mode-line-format nil)))
  (vterm-mode . centaur-tabs-local-mode)
  :general
  ("C-\\" #'vterm)
  :ensure t)

(provide 'init-terminal)
;;; init-terminal.el ends here
