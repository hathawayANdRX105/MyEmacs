;;; package --- summary
;;; commentary:
;;; init-other contains some unsorted package configurations.

;;; Code:

;; it's used to mesure emacs startup time.
;; (use-package benchmark-init
;;   :disabled t
;;   ;; :hook (after-init-hook . benchmark-init/deactivate)
;;   :init (add-hook 'after-init-hook #'benchmark-init/deactivate))

(use-package gcmh
  :demand t
  :init
  (setq gcmh-high-cons-threshold 33554432)
  (gcmh-mode 1))

(provide 'init-other)
;;; init-other.el ends here.
