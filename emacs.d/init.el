;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(package-activate-all)

(require 'use-package)
(setq use-package-always-ensure t)


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

(tool-bar-mode -1)    ;; disable the big ugly tool bar icons
(scroll-bar-mode -1)  ;; disable the scroll bar
(global-linum-mode 0)  ;; Do not use linum mode in Emacs v26+ (slow), use instead:
(add-hook 'prog-mode-hook 'display-line-numbers-mode)  ;; only during programming


;; Store all backup-files in a single folder in the emacs user directory
(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))


;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))


;; Dired settings:
;; To enable dired to list stuff on OS X
;; you also need to install coreutils (through brew) for this to work.
;; This can be done with the command brew install coreutils
(when (equal system-type 'darwin)
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))



;; The first line enables the functionality to open folders
;; in the same dired buffer. The second and third lines take
;; it a step further and bind the enter key in dired to
;; dired-find-alternate-file so that opening folders in
;; the same buffer becomes the defaut behaviour.
;; Emacs executes these lines after the dired package is loaded.

;; Open dired in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")


;; =========
;; Work machine settings
;; Pandoc for markdown conversion
;; (setq markdown-command "~/.emacs.d/pandoc/pandoc.exe")
;; Location of python interpreter on work machine
;; (setq python-shell-interpreter "~/.emacs.d/python-3.8.9-embed-win32/python")
;; ==========


;; Some usefull keybinings
(global-set-key (kbd "M-ö") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))


;; IDO settings (with recetf and also smex)
(ido-mode 1)
(setq ido-enable-flex-matching t)

(use-package smex
  :config
  (smex-initialize)
  ;; will possibly be overruled later by counsel settings
  (global-set-key (kbd "M-x") 'smex) 
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; word wrap in text und org-mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)


;; Display line numbers in text and org files :-)
(setq display-line-numbers-width 5)
;; (add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package undo-tree
  :config
  (global-undo-tree-mode)
  ;; Settings to complement undo tree
  (global-set-key (kbd "C-z") 'undo)  ;; make ctrl-z undo
  (defalias 'redo 'undo-tree-redo)    ;; make ctrl-S-z redo
  (global-set-key (kbd "C-S-z") 'redo))


;; Old auto-complete settings
 ;; (use-package auto-complete
 ;;   :config
 ;;   (ac-config-default)
 ;;   (setq ac-ignore-case nil)
 ;;   (setq ac-delay 0.05)
 ;;   (setq ac-auto-show-menu 0.05))


;; which key for displaying key combinations at the bottom
(use-package which-key
  :config
  (which-key-mode))


(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.05))


(use-package counsel
  ;; will install ivy and swiper as dependencies
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")  ;; intentional space before end of string
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((swiper-isearch . ivy--regex-plus)     ;; regex plus matching for swiper
          (ivy-switch-buffer . ivy--regex-plus)  ;; and switch-buffer
          (t . ivy--regex-plus)))                ;; and for evything else
  ;; Ivy-based interface to standard commands
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-x b") 'counsel-ibuffer)
  ;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  ;; Ivy-resume and other commands (ivy-resume resumes the last Ivy-based completion).
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c b") 'counsel-bookmark)
  (global-set-key (kbd "C-c d") 'counsel-descbinds)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c o") 'counsel-outline)
  (global-set-key (kbd "C-c t") 'counsel-load-theme)
  (global-set-key (kbd "C-c F") 'counsel-org-file))


(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol))


;; Everyone needs some magit
(use-package magit)


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Repos/")
    (setq projectile-project-search-path '("~/Repos/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


;; A somewhat nicer modeline
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; Org Mode Settings
;; Time stamp after finished todo.
(setq org-log-done t)

;; Use relative path for files in the current directory and sub- directories of it.
;; For other files, use an absolute path.
(setq org-link-file-path-type 'adaptive)

;; === Taken from Emacs From Scratch Config ===
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org-faces  ;; Otherwise it's throws an invalid face error.
  (dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Roboto" :weight 'medium :height (cdr face))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)


;; Personal functions folder
(add-to-list 'load-path "~/.emacs.d/personal/")


;; ==========
;; Testing of using Emacs nano
(add-to-list 'load-path "~/.emacs.d/personal/nano-emacs-master/")

(setq nano-font-size 11)
;; (load "nano")

;; ==========

;; Stuff added by Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(custom-enabled-themes '(dichromacy))
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" default))
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-tail-colors '(("#aecf90" . 0) ("#c0efff" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(mouse-wheel-progressive-speed nil)
 '(ns-right-alternate-modifier 'none)
 '(org-agenda-files '("~/Dropbox/_org/journal.org"))
 '(package-selected-packages
   '(counsel-projectile projectile magit all-the-icons markdown-mode which-key use-package undo-tree smex rainbow-delimiters org-bullets helpful counsel company auto-complete))
 '(pdf-view-midnight-colors '("#282828" . "#f2e5bc"))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#a60000")
     (40 . "#721045")
     (60 . "#8f0075")
     (80 . "#972500")
     (100 . "#813e00")
     (120 . "#70480f")
     (140 . "#5d3026")
     (160 . "#184034")
     (180 . "#005e00")
     (200 . "#315b00")
     (220 . "#005a5f")
     (240 . "#30517f")
     (260 . "#00538b")
     (280 . "#093060")
     (300 . "#0031a9")
     (320 . "#2544bb")
     (340 . "#0000c0")
     (360 . "#5317ac")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "#f0f0f0"])
 '(xterm-color-names-bright
   ["#505050" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
