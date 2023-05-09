;;; package --- summary
;;; commentary:
;;; init-completion contains some package configurations used for completions.

;;; Code:


;; completion filter support
(use-package orderless
  :defer 1
  :config
  ;; use cheap prefix filtering to sort candidates
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-dispatch '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless-fast)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

;; fuzzy filter support
(use-package fussy
  :defer 2
  :config
  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil))

(use-package flx-rs
  :disabled t
  :after fussy
  :config
  (setq fussy-score-fn 'flx-rs-score)
  (flx-rs-load-dyn))


;; Enable vertico for M-x helpful display
(use-package vertico
  :defer 2
  :init
  (vertico-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; explatin M-x command
(use-package marginalia
  :defer 5
  :init
  (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)))


(use-package emacs
  :defer 2
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))


(use-package corfu
  :disabled t
  :after orderless
  :load-path "straight/build/corfu/extensions"
  :hook
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
  :bind
  (:map corfu-map
        ("<escape>"	.	#'corfu-quit)
        ("C-<return>"	.	#'corfu-quick-complete)
        ("M-<space>"	.	#'corfu-insert-separator)
        ("<tab>"	.	#'corfu-complete))

  :config
  (global-corfu-mode)
  (setq completion-cycle-threshold 3)
  
  (require 'corfu-history)
  (corfu-history-mode)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; (add-to-list 'load-path (expand-file-name "straight/build/corfu/extensions" user-emacs-directory)

;; icons of completions
(use-package kind-icon
  :disabled t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style
   '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; completions backends
(use-package cape
  :disabled t
  :after corfu
  :config
  (setq-local add-to-completion (lambda(cape-backend) (add-to-list 'completion-at-point-functions cape-backend)))
  (mapc add-to-completion '(cape-keyword cape-dabbrev cape-file)))


;; Snippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :bind ("M-`" . #'yas-expand)
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
  :disabled t
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
  :disabled t
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
  :disabled t
  :after lsp-mode
  :commands (consult-lsp-diagnostics
             consult-lsp-symbols
             consult-lsp-file-symbols))


;; syntax checker
(use-package flycheck
  :disabled t
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






;; (add-to-list 'load-path (expand-file-name "quelpa/build/lsp-bridge" user-emacs-directory))

;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

(use-package lsp-bridge
  :defer 1
  :after yasnippet
  :ensure nil
  :demand t
  :quelpa (lsp-bridge :fetcher github :repo "manateelazycat/lsp-bridge")
  :init
  (add-to-list 'load-path (expand-file-name "quelpa/build/lsp-bridge" user-emacs-directory))
  ;; :hook (acm-mode . (lambda () (setq-local mode-line-format nil)))
  :custom
  ;; (acm-candidate-match-function 'orderless-prefixes)
  (acm-candidate-match-function 'flx-find-best-match)
  (acm-enable-quick-access t)
  (acm-enable-tempel nil)
  (acm-enable-doc nil)
  (lsp-bridge-enable-auto-format-code t)
  (lsp-bridge-diagnostics-fetch-idle 0.5)
  (lsp-bridge-auto-format-code-idle 15)
  
  :config
  (yas-global-mode 1)
  (global-lsp-bridge-mode)
  
  :bind
  (("<f2>" . #'lsp-bridge-rename)
  :map acm-mode-map
  ("M-j" . #'lsp-bridge-popup-documentation-scroll-down)
  ("M-k" . #'lsp-bridge-popup-documentation-scroll-up)
  :map meow-normal-state-keymap
  ("M-d". #'lsp-bridge-popup-documentation)
  ("/ d" . lsp-bridge-find-def)
  ("/ D" . 'lsp-bridge-find-def-return)

  ("/ a" . 'lsp-bridge-code-action)
  ("/ i" . 'lsp-bridge-find-impl)
  ("/ r" . 'lsp-bridge-find-references)
  ("/ h" . 'lsp-bridge-signature-help-fetch)
  ("/ g" . 'lsp-bridge-list-diagnostics)
  ("/ 1" . 'lsp-bridge-jump-to-prev-diagnostic)
  ("/ 2". 'lsp-bridge-jump-to-next-diagnostic)))


(provide 'init-completion)

;;; init-completion.el ends here
