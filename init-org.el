(add-to-list 'load-path "~/.emacs.d/org-7.9.3d/lisp/")
(add-to-list 'load-path "~/.emacs.d/org-7.9.3d/contrib/lisp" t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-tag-alist '(("@efficiency" . ?e) ("@duty" . ?d) ("career" . ?c)))
(setq org-log-done 'note)



(provide 'init-org)