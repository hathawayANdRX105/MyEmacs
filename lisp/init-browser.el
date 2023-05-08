;;; package --- summary
;;; commentary:
;;; init-browser contains package configuration used for browsing projects or codes.

;;; Code:


(use-package dirvish
  :defer 1
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                         "Home")
     ("d" "~/.emacs.d/"                "Emacs")
     ("p" "~/projects/"                "Drives")))
  :hook
  (dirvish-mode			.	(lambda () (setq-local mode-line-format nil)))
  (dirvish-mode-activtion	.	(lambda () (setq-local mode-line-format nil)))
  (dirvish-mode-deactivtion	.	(lambda () (setq-local mode-line-format nil)))
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  
  (if (display-graphic-p)
      (setq dirvish-attributes
            '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
    (setq dirvish-attributes
          '(vc-state subtree-state collapse git-msg file-time file-size)))

  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("M-1" . dirvish-side)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("h"					.	dired-up-directory)
   ("j"					.	dired-next-line)
   ("k"					.	dired-previous-line)
   ("l"					.	dired-find-file)
   ("i"					.	wdired-change-to-wdired-mode)
   ("."					.	dired-omit-mode)
   ("TAB"				.	dirvish-subtree-toggle)
   ("M-s"				.	dirvish-setup-menu)
   ("M-f"				.	dirvish-toggle-fullscreen)
   ("*"					.	dirvish-mark-menu)
   ("b"					.	dirvish-bookmark-goto)
   ("f"					.	dirvish-file-info-menu)
   ([remap dired-sort-toggle-or-edit]	.	dirvish-quicksort)
   ([remap dired-do-redisplay]		.	dirvish-ls-switches-menu)
   ([remap dired-summary]		.	dirvish-dispatch)
   ([remap dired-do-copy]		.	dirvish-yank-menu)
   ([remap mode-line-other-buffer]	.	dirvish-history-last)))



;; file tabs
(use-package centaur-tabs
  :defer 2
  :after meow
  :hook
  (dirvish-mode		.	centaur-tabs-local-mode)
  (org-src-mode		.	centaur-tabs-local-mode)
  (term-mode		.	centaur-tabs-local-mode)
  (calendar-mode	.	centaur-tabs-local-mode)
  (org-agenda-mode	.	centaur-tabs-local-mode)
  (helpful-mode		.	centaur-tabs-local-mode)
  (dashboard-mode	.	centaur-tabs-local-mode)
  (prog-mode		.	centaur-tabs-mode) ;lazy load centaur-tabs
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-set-icons nil)
  ;; (centaur-tabs-set-bar 'under)
  :init
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :config
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-mode t)
  :general
  ("M-<left>"  #'centaur-tabs-backward)
  ("M-<right>"  #'centaur-tabs-forward)
  ("M-<down>"  #'centaur-tabs-switch-group))

(provide 'init-browser)
;;; init-browser.el ends here
