;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(defvar emacs-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Emacs is powering up... Be patient, Master %s!" emacs-user)


(prefer-coding-system 'utf-8)               ;; UTF-8 for everything
(define-coding-system-alias 'UTF-8 'utf-8)  ;; Uppercase is same as lowercase

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq visible-bell t)  ;; visible bell (instead the annoying warning noise; beware of the mac)

;; These are set in customs
;; (tool-bar-mode -1)    ;; disable the big ugly tool bar icons
;; (scroll-bar-mode -1)  ;; disable the scroll bar
;; (global-linum-mode 0)  ;; Do not use linum mode in Emacs v26+ (slow), use instead:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)  ;; only during programming

;; Store all backup-files in a single folder in the emacs user directory
(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))



(defvar emacs-dir (file-name-directory load-file-name)
  "Get the path to .emacs.d. The root dir of the Emacs configuration.")

(defvar personal-dir (expand-file-name "personal" emacs-dir)
  "This directory is for your personal configuration.
   All Emacs Lisp files there are loaded automatically")

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (message "Emacs is loading the personal configuration files in %s..." personal-dir)
  (mapc 'load (directory-files personal-dir 't "^[^#\.].*\\.el$"))) ;; probably mapc can be deleted



;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

