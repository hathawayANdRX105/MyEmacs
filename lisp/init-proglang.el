;;; package --- summary
;;; commentary:
;;; init-proglang contains package configuration used for browsing projects or codes.

;;; Code:
;; code folder

(with-eval-after-load 'general
  (defun toggle-hiding ()
    (interactive)
    (if (hs-already-hidden-p)
        (hs-show-block)
      (hs-hide-block)))
  
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (general-define-key
   [remap hs-toggle-hiding] 'toggle-hiding))


;; bracket supplement.
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))


(use-package rust-mode
  :hook
  (rust-mode . (lambda () (prettify-symbols-mode)))
  :config
  (setq rust-format-on-save t)
  (with-eval-after-load 'lsp-mode
    (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))))
  

(provide 'init-proglang)
;;; init-proglang.el ends here
