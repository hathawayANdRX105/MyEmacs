;;; package --- summary
;;; commentary:
;;; init-default-config contains some basic Emacs configurations.

;;; Code:

;; encoding
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; auto save 
(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves


;; backup
(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name ".cache/backups" user-emacs-directory)))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files t       ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash


;; bookmark
(setq bookmark-default-file (expand-file-name ".cache/bookmark" user-emacs-directory))

(setq transient-levels-file (expand-file-name ".cache/transient/levels.el" user-emacs-directory)
      transient-values-file (expand-file-name ".cache/transient/values.el" user-emacs-directory)
      transient-history-file (expand-file-name ".cache/transient/history.el" user-emacs-directory))


;; recent file
(setq recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory)
      project-list-file (expand-file-name ".cache/projects" user-emacs-directory)
      savehist-file (expand-file-name ".cache/history" user-emacs-directory))

(require 'recentf)  ;; it is a built-in package
(recentf-mode 1)
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100)



;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))


;; window divider
(setq window-divider-default-right-width 0
      window-divider-default-places 'right-only)
(window-divider-mode 1)

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

;; alpha setting
(progn
  (set-frame-parameter (selected-frame) 'alpha '(95))
  (set-face-background 'line-number (face-attribute 'default :background))
  (set-face-background 'fringe (face-attribute 'default :background)))

;; tab space setting
(setq default-tab-width 4)
(setq indent-tabs-mode nil)

(provide 'init-default-config)
;;; init-default-config.el ends here
