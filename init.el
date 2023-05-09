;;; package --- summary
;;; commentary:
;;; Code:

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; config load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load main configuration 
(require 'init-elpa)
(require 'init-default-config)
(require 'init-ui)

(require 'init-keymappings)
(require 'init-search)
(require 'init-completion)

(require 'init-elemacs)

;; load some org package with elemacs-incremental-packages
(elemacs-load-packages-incrementally
 '(calendar find-func format-spec org-macs org-compat
	    org-faces org-entities org-list org-pcomplete org-src
	    org-footnote org-macro ob org org-clock org-agenda org-capture))

(elemacs-load-packages-incrementally
 '(eldoc easymenu help-mode ffap
	 dash f s git-commit package eieio transient))

(elemacs-load-packages-incrementally
 '(init-hl init-navigation init-proglang init-search
	   init-browser init-session init-terminal
	   init-other))

;;; init.el ends here
