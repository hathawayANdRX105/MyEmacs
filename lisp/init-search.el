;;; package --- summary
;;; commentary:
;;; init-search contains some search packages' configurations.

;;; Code:


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
