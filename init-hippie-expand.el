(setq hippie-expand-try-functions-list 
'(try-expand-line 
try-expand-line-all-buffers 
try-expand-list 
try-expand-list-all-buffers 
try-expand-dabbrev 
try-expand-dabbrev-visible 
try-expand-dabbrev-all-buffers 
try-expand-dabbrev-from-kill 
try-complete-file-name 
try-complete-file-name-partially 
try-complete-lisp-symbol 
try-complete-lisp-symbol-partially 
try-expand-whole-kill)) 

(global-set-key (kbd "C-c /") 'hippie-expand) 
(provide 'init-hippie-expand)
