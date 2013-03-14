;; ----------------------------------------------------
;;  winner-mode undo / redo for window layout
;; -----------------------------------------------------

(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "<f11> C-z") 'winner-undo)
(global-set-key (kbd "<f11> C-y") 'winner-redo)

;; -----------------------------------------------------
;; modified from windmove-do-window-select
;; -----------------------------------------------------

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; -----------------------------------------------------
;; toggle-full-screen
;; -----------------------------------------------------

(defun toggle-full-screen (arg)
  (interactive "P")
  (cond
   ( (and (eq window-system 'w32)             
          (locate-file "emacs_fullscreen.exe" exec-path nil 'file-executable-p))
     (shell-command "emacs_fullscreen.exe") )
   ( (display-graphic-p)
     (let ((current-value (frame-parameter nil 'fullscreen)))
       (set-frame-parameter nil 'fullscreen
                            (if (equal 'fullboth current-value)
                                (if (boundp 'old-fullscreen) old-fullscreen nil)
                              (progn
                                (setq old-fullscreen current-value)
                                'fullboth)))) )
   (t
    (or (require 'maxframe nil t)
        (require 'fit-frame nil t))
    (if (fboundp 'maximize-frame)
        (if menu-bar-mode
            (call-interactively 'restore-frame)
          (call-interactively 'maximize-frame))
      (message "Failed to find a way to toggle full screen."))))
   (when arg
     (tool-bar-mode (not menu-bar-mode))
     (menu-bar-mode (not menu-bar-mode))
     (scroll-bar-mode (not menu-bar-mode)))
   )

(global-set-key (kbd "<f11> M-RET") 'toggle-full-screen)

(provide 'init-window)
