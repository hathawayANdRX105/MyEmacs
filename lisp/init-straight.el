;;; package --- summary
;;; commentary:
;;; init-straight will bootstrap install straith.el

;;; Code:

(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
      package-check-signature nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; install use-package
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
(provide 'init-straight)
;;; init-straight.el ends here
