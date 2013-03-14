

(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'qiang-comment-dwim-line)

(add-to-list 'load-path "~/.emacs.d/doxymacs/no-autoconf/")
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

;; C-c d ?	will look up documentation for the symbol under the point.	查找当前鼠标点下的符号的文档
;; C-c d r	will rescan your Doxygen tags file.	                        重新扫描tags文件
;; C-c d f	will insert a Doxygen comment for the next function.     	为函数插入Doxygen注释
;; C-c d i	will insert a Doxygen comment for the current file.	        为文件插入Doxygen注释
;; C-c d ;	will insert a Doxygen comment for the current member.	        为当前成员插入Doxygen注释
;; C-c d m	will insert a blank multiline Doxygen comment.	                插入多行注释
;; C-c d s	will insert a blank singleline Doxygen comment.         	插入单行注释
;; C-c d @	will insert grouping comments around the current region.	插入环绕当前区域的注释

(provide 'init-comment)