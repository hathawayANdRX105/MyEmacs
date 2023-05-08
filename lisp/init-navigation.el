
;; edit jumper
(use-package avy
  :commands (avy-goto-char))

;; window picker
(use-package ace-window
  :commands (ace-window))


;; window mover
;; window move left => C-left
;; window move right => C-right
;; window move up => C-up
;; window move down => C-down

(windmove-default-keybindings 'control)
(with-eval-after-load 'general
  (general-define-key
   :keymaps '(prog-mode-map org-mode-map minibuffer-mode-map meow-motion-state-keymap eshell-mode-map help-mode-map vterm-mode-map)
   "C-h" 'windmove-left
   "C-l" 'windmove-right
   "C-k" 'windmove-up
   "C-j" 'windmove-down))

(provide 'init-navigation)
 
