;;; package --- summary
;;; commentary:
;;; init-keymappings contains some keymap binding package configurations.

;;; Code:

;; keymapping definer
(use-package general
  :demand t
  :config
  (general-define-key
   :keymaps '(prog-mode-map org-mode-map minibuffer-mode-map meow-motion-state-keymap eshell-mode-map help-mode-map vterm-mode-map)
   [remap list-buffers] #'consult-buffer
   [remap load-theme]   #'consult-theme
   "M-w"                #'ace-window
   "M-e"                #'centaur-tabs-ace-jump

   ;; control buffer
   "M-i"                #'ibuffer
   "M-k"                #'ace-delete-window
   
   ;; "M-/"		#'set-mark-command
   "C-;"                #'comment-line)
  
  ;; don't suspend emacs frame.
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  )


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-normal-define-key
   '("=" . meow-indent)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . quit-window)
   '("Q" . kill-this-buffer)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . kill-ring-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("<" . beginning-of-buffer)
   '(">" . end-of-buffer))
  
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  
  ;; leader keymapping SPC
  (meow-leader-define-key
   ;; set prefix for SPC-
   ;; m -> M-
   ;; g -> C-M-
   ;; x / h / c -> C-x / C-h / C-c

   ;; file & buffer
   '("f" . consult-find)
   '("r" . consult-recent-file)
   '("b" . consult-buffer)
   '("B" . consult-buffer-other-window)
   '("s" . consult-line)
   '("S" . consult-ripgrep)
   
   '("E" . eval-buffer)


   '("-" . hs-hide-block)
   '("=" . hs-show-block)
   '("TAB" . hs-toggle-hiding)

   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)

   ;; navigation
   '("j" . avy-goto-char)

   ;; use emacs built-in project.el replace projectile
   '("p a" . project-remember-projects-under)
   '("p r" . project-forget-projects-under)
   '("p p" . project-switch-project)
   '("p B" . project-swith-buffer)
   '("p f" . project-find-file)
   '("p q" . project-query-replace-regexp)))

;; main keymapping
(use-package meow
  :demand t
  :custom
  (meow-use-clipboard t)
  :config
  (meow-setup)
  (meow-global-mode 1)

  (with-eval-after-load 'doom-modeline
    (setq meow-replace-state-name-list
          '((normal . "n")
            (insert . "i")
            (keypad . "k")
            (motion . "m")))))


(use-package which-key
  :defer 2
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(provide 'init-keymappings)
;;; init-keymappings.el ends here
