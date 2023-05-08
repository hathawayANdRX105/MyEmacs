;;; package --- summary
;;; commentary:
;;; init-completion contains some package configurations used for completions.

;;; Code:


(use-package corfu
  :defer 2
  :hook
  ;; (minibuffer-setup	.	corfu-mode)
  ;; (minibuffer-setup	.	corfu-enable-in-minibuffer)
  (eshell-mode . (lambda ()
		   (setq-local corfu-auto t)
		   (corfu-mode)
		   (setq buffer-display-table (make-display-table))))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match t)
  (corfu-echo-documentation t)
  (corfu-on-exact-match 'quit)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-hide nil)

  ;; :init
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;     (corfu-mode 1)))

  :bind
  (:map corfu-map
        ("<escape>"	.	#'corfu-quit)
        ("C-<return>"	.	#'corfu-quick-complete)
        ("M-<space>"	.	#'corfu-insert-separator)
        ("<tab>"	.	#'corfu-complete))

  :config
  (setq completion-cycle-threshold 3)
  ;; (setq tab-always-indent 'complete)
  (corfu-history-mode t)
  (global-corfu-mode))


;; icons of completions
(use-package kind-icon
  ;;:demand t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style
   '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



;; completions backends
(use-package cape
  :demand t
  :after corfu
  :config
  (setq-local add-to-completion (lambda(cape-backend) (add-to-list 'completion-at-point-functions cape-backend)))
  (mapc add-to-completion '(cape-keyword cape-dabbrev cape-file)))

;; Snippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (("M-`" . #'yas-expand))
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package yasnippet-snippets
  :disabled t
  :defer t
  :after yasnippet)


;; language server
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-place-position 'above-line)
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-code-action-fallback-icon (all-the-icons-faicon "gamepad"))
  (lsp-restart t)
  (lsp-session-file (expand-file-name ".cache/.lsp-session-v1" user-emacs-directory))

  :init
  (defun orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (add-hook 'orderless-style-dispatchers #'orderless-dispatch-flex-first nil 'local)

  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-completion-mode-hook  #'lsp-mode-setup-completion)

  :general
  (:keymaps '(meow-normal-state-keymap)
            "<f2>"  'lsp-rename
            "/ D"  'lsp-find-definition
            "/ R"  'lsp-find-references
            "/ f"  'lsp-format-buffer
            "/ a"  'lsp-execute-code-action
            "/ o"  'lsp-organize-imports
            "/ g"  'consult-lsp-diagnostics
            "/ s"  'consult-lsp-symbols
            "/ S"  'consult-lsp-file-symbols)
  :config
  (setq lsp-enable-completion-at-point t
        lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        ;; lsp-enable-semantic-highlighting nil
        ;; lsp-enable-symbol-highlighting nil
        ;; lsp-enable-text-document-color nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  (setq lsp-prefer-capf t
        lsp-enable-snippet t
        lsp-lens-enable t)
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-log-io nil)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docs\\'")
  (setq lsp-modeline-diagnostics-scope :workspace)

  ;; setup lsp completions
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  ;; solve conflict with noninterruptible problem.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))


(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-delay 5)
  ;; (lsp-ui-doc-position 'top)
  :general
  (:keymaps '(meow-normal-state-keymap)
            "/ r" #'lsp-ui-peek-find-references
            "/ d" #'lsp-ui-peek-find-definitions
            "/ i" #'lsp-ui-imenu))

(use-package consult-lsp
  :after lsp-mode
  :commands (consult-lsp-diagnostics
             consult-lsp-symbols
             consult-lsp-file-symbols))


;; syntax checker
(use-package flycheck
  :hook (lsp-mode . global-flycheck-mode)
  :init
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
              org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'left-fringe
                                   'left-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled idle-change))

  :custom
  (flycheck-error-list-minimum-level 'warning)
  (flycheck-idle-change-delay 0.5)
  :general
  ('(meow-normal-state-keymap)
   "/ c" #'lsp-ui-flycheck-list))


(provide 'init-completion)

;;; init-completion.el ends here
