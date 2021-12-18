;; Some usefull keybinings
(global-set-key (kbd "M-ö") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)


;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))


(use-package undo-tree
  :config
  (global-undo-tree-mode)
  ;; Settings to complement undo tree
  (global-set-key (kbd "C-z") 'undo)  ;; make ctrl-z undo
  (defalias 'redo 'undo-tree-redo)    ;; make ctrl-S-z redo
  (global-set-key (kbd "C-S-z") 'redo))


;; Visual line mode in text and org files :-)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
;; (add-hook 'text-mode-hook 'auto-complete-mode)
(setq display-line-numbers-width 5)


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


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
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-x b") 'counsel-ibuffer)
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
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


;; Location of python interpreter on work machine
;; (setq python-shell-interpreter "~/.emacs.d/python-3.8.9-embed-win32/python")
