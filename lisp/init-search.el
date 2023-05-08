;;; package --- summary
;;; commentary:
;;; init-search contains some search packages' configurations.

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

;; the below are dependecy packages && extra settings
;; this package does not need to install, it is part of vertico.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package savehist
  :defer 5
  :init
  (savehist-mode))

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

;; explatin M-x command
(use-package marginalia
  :defer 5
  :init
  (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; main search engine
(use-package consult
  :commands (consult-recent-file
             consult-locate
             consult-line
	     switch-to-buffer)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
;;  (consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)


  :config
  (setq completion-styles '(substring basic))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))


;; emacs help enchacement
(use-package helpful
  :commands (helpful-variable
             helpful-command
             helpful-function
             helpful-key)
  :bind
  ([remap describe-function] . #'helpful-function)
  ([remap describe-command]  . #'helpful-command)
  ([remap describe-variable] . #'helpful-variable)
  ([remap describe-key]      . #'helpful-key))

(provide 'init-search)
;;; init-search.el ends here.
