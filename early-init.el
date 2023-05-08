;;; package --- summary
;;; Commentary:
;;   early eamcs initlization configuration.

;;; Code:

(setq package-enable-at-startup nil)
(setq inhibit-compacting-font-caches t)

(setq load-prefer-newer noninteractive)

;; garbage optimize
(setq gc-cons-threshold most-positive-fixnum)


(defvar temp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)



(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 100000000 ; 100mb
		  gc-cons-percentage 0.1)
	    (setq file-name-handler-alist temp-file-name-handler-alist)
	    (setq read-process-output-max (* 1024 1024))))

(add-hook 'minibuffer-setup-hook (lambda() (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook (lambda()
				  (run-at-time 1 nil (lambda ()
						       (setq gc-cons-threshold 16777216)))))



;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))


;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)


;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
;; inhibit
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      inhibit-compacting-font-caches t)


;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5


;; ui interface
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)


(setq pop-up-windows nil)
(setq use-short-answers t)
(setq fill-column 80)
(setq ring-bell-function 'ignore)


(provide 'early-init-file)

;;; early-init.el ends here
