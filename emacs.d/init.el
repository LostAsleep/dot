;; Initialize Package system and installs
;; ======================================
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; If there are no archived package contents, refresh them
(unless package-archive-contents (package-refresh-contents))

;; Installs packages - myPackages contains a list of package names  ;;;;;
(defvar myPackages
  '(
    all-the-icons
    auto-complete
    dashboard
    helpful
    org
    projectile
    undo-tree
    smex
    which-key
    exec-path-from-shell  ;; set Emacs' `exec-path' and $PATH from the shell path
    elpy
    company
    markdown-mode
    ))
 
;; Scans myPackages - If a package is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
 
;; Requre all packages
(mapc #'(lambda (package)
           (require package))
      myPackages)


;; Some general settings
;; =====================
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
(setq ns-right-alternate-modifier 'none)  ;; Important for MacOS

(tool-bar-mode -1)    ;; disable the big ugly tool bar icons
(scroll-bar-mode -1)  ;; disable the scroll bar
(global-linum-mode 0)  ;; Do not use linum mode in Emacs v26+ (slow), use instead:
(add-hook 'prog-mode-hook 'display-line-numbers-mode)  ;; only during programming

;; Store all backup-files in a single folder in the emacs user directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Some usefull keybindings
(global-set-key (kbd "M-ö") #'hippie-expand)
(global-set-key (kbd "M-o") #'other-window)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(show-paren-mode)

;; word wrap in text und org-mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;; Use shell's $PATH
;; =================
(exec-path-from-shell-copy-env "PATH")

;; ido and smex
;; ============
;; recentf
(require 'recentf)
(setq recentf-save-file "~/.recentf")
(recentf-mode 1)
(setq recentf-max-menu-items 20
      recentf-max-saved-items 1000)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(ido-everywhere)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; undo-tree
;; =========
(global-undo-tree-mode)
;; Settings to complement undo tree
(global-set-key (kbd "C-z") 'undo)  ;; make ctrl-z undo
(defalias 'redo 'undo-tree-redo)    ;; make ctrl-S-z redo
(global-set-key (kbd "C-S-z") 'redo)


;; which key for displaying key combinations
;; =========================================
(which-key-mode)


;; helpful for better help buffers
;; ================================
(defalias #'describe-key #'helpful-key)
(defalias #'describe-function #'helpful-callable)
(defalias #'describe-variable #'helpful-variable)
(defalias #'describe-symbol #'helpful-symbol)


;; Org Mode Settings
;; =================
;; Time stamp after finished todo.
(setq org-log-done t)

;; Use relative path for files in the current directory and sub- directories of it.
;; For other files, use an absolute path.
(setq org-link-file-path-type 'adaptive)

;; to enable resizing of image display
(setq org-image-actual-width nil)


;; Projectile
;; ==========
(require 'projectile)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(setq projectile-switch-project-action #'projectile-dired)


;; Dashboard
;; =========
(require 'all-the-icons)

(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        ;(registers . 5)
			))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)


;; auto-complete
;; =============
(require 'auto-complete)
(ac-config-default)
(setq ac-candidate-limit 1000)
(setq ac-ignore-case nil)
(setq ac-delay 0.05)
(setq ac-auto-show-menu 0.05)
(with-eval-after-load 'auto-complete
  (ac-flyspell-workaround))


;; Spell checking
;; ==============
(setq ispell-program-name "/usr/local/bin/aspell")


;; Rest found here: https://www.tenderisthebyte.com/blog/2019/06/09/spell-checking-emacs/
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;; Not for those two modes:
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; If you’re using a Mac, you may need to add the following Elisp code
;; in order for Flyspell to pick up the two-finger clicks (right-clicks):
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; Python Development
;; ==================
(setq python-shell-interpreter "python3")

;;Enable elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; CUSTOM FUNCS
;; ============
(defun phi/new-dashboard ()
  "Jump to the dashboard buffer, if it does not exist create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))


;; CUSTOM FILE
;; ===========
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
