;;; package --- summary
;;; commentary:
;;; init-elpa contains some package configurations .

;;; Code:

(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
      package-check-signature nil)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(provide 'init-elpa)
;;; init-elpa.el ends here
