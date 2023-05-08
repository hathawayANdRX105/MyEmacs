;;; package --- summary
;;; commentary:
;;; init-ui-default contains some ui builtin configurations.

;;; Code:

(defun setup-my-fontset()

  ;; english fontset
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font" :weight 'regular :height 170)
  
   (set-face-attribute 'italic nil
                       :family "JetBrainsMono Nerd Font" :weight 'semilight :slant 'italic)

  ;;chinese fontset "YaHei Consolas Coder"
;;  (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                    charset
;;                      (font-spec :family "更纱黑体 Mono SC Nerd regular")))
;;  (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "更纱黑体 Mono SC Nerd regular" :size 16) nil 'prepend)

)

(add-hook 'window-setup-hook #'setup-my-fontset)
(add-hook 'server-after-make-frame-hook #'setup-my-fontset)


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

;; icons
(use-package all-the-icons
  :demand t
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :preface
  (add-hook 'after-setting-font-hook
	    (lambda ()
	      (dolist (font (list "Weather Icons"
				  "github-octicons"
				  "FontAwesome"
				  "all-the-icons"
				  "file-icons"
				  "Material Icons"))
		(set-fontset-font t 'unicode font nil 'append)))))


;; don't ask every time for load new theme.
(setq custom-safe-themes t)

;; line spacing
(setq-default line-spacing 0)
(setq widget-image-enable nil)

;; display line number and hilight current line.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)




;; modeline
(use-package doom-modeline
  ;; :after all-the-icons
  :hook
  (prog-mode	.	doom-modeline-mode)
  (org-src-mode .	(lambda () (setq-local mode-line-format nil)))
  :custom
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-buffer-name t)
  (doom-modeline-icon nil)
  (doom-modeline-modal-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-lsp t) ;
  (doom-modeline-github t)					
  ;; (doom-modeline-height 12)
  (doom-modeline-bar-width 0)
  :config
  ;; modeline display current time
  (setq display-time-format "%H:%M")
  (display-time-mode 1))




(provide 'init-ui)
;;; init-ui-default.el ends here.
