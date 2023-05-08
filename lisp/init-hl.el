;;; package --- summary
;;; commentary:
;;; init-hl contains some builtin package configurations .

;;; Code:

;; rainbow bracket
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; highlight diff
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode))

;; enhance symbol highlight
;; add hook to lazy load packages below.
(use-package tree-sitter
  :hook (rust-mode . tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(provide 'init-hl)
;;; init-hl.el ends here
