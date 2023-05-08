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


(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(require 'quelpa)

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

(use-package quelpa-use-package
  :demand t
  :config
  (setq use-package-ensure-function 'quelpa))

;; (cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
;;   "Install a package from a remote if it's not already installed.
;; This is a thin wrapper around `package-vc-install' in order to
;; make non-interactive usage more ergonomic.  Takes the following
;; named arguments:

;; - FETCHER the remote where to get the package (e.g., \"gitlab\").
;;   If omitted, this defaults to \"github\".

;; - REPO should be the name of the repository (e.g.,
;;   \"slotThe/arXiv-citation\".

;; - NAME, REV, and BACKEND are as in `package-vc-install' (which
;;   see)."
;;   (let* ((url (format "https://www.%s.com/%s" fetcher repo))
;;          (iname (when name (intern name)))
;;          (pac-name (or iname (intern (file-name-base repo)))))
;;     (unless (package-installed-p pac-name)
;;       (package-vc-install url iname rev backend))))

(provide 'init-elpa)
;;; init-elpa.el ends here
