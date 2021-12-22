;; IDO settings (with recetf and also smex)
;; Code:

(ido-mode 1)
(setq ido-enable-flex-matching t)
;; and also use it for finding files (can be intrusive)
;; (setq ido-everywhere t)
;; force Ido to always create a new buffer (in C-x b) if it does not exist
(setq ido-create-new-buffer 'always)


(require 'recentf)
;; get rid of `find-file-read-only' and replace it with something more useful
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(recentf-mode t)  ;; enable recent files mode
(setq recentf-max-saved-items 50)  ; 50 files ought to be enough
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
    (message "Opening file...")
    (message "Aborting")))


(use-package smex
  :config
  (smex-initialize)
  ;; will possibly be overruled later by counsel settings
  (global-set-key (kbd "M-x") 'smex) 
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; (use-package fancy-dabbrev
;;   :config
;;   (defun activate-fancy-dabbrev-keys ()
;;     "Set the fancy-dabbrev keys localy and hook to mode."
;;     (local-set-key (kbd "<tab>") 'fancy-dabbrev-expand-or-indent)
;;     ; If you want TAB to indent the line like it usually does when the cursor
;;     ; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
;;     (local-set-key (kbd "<backtab>") 'fancy-dabbrev-backward))
;;   (add-hook 'fancy-dabbrev-mode-hook 'activate-fancy-dabbrev-keys)
;;   (setq dabbrev-case-distinction t)
;;   (setq dabbrev-case-fold-search t)
;;   (setq dabbrev-case-replace nil))
