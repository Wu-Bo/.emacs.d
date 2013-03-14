;; 去除shell mode的行号
(setq inhibit-linum-mode-alist
      '(
        eshell-mode
        shell-mode
        term-mode
        ))

(defadvice linum-on (around inhibit-for-modes activate)
  "Stop turing linum-mode if it is in the inhibit-linum-mode-alist."
  (unless (member major-mode inhibit-linum-mode-alist)
    ad-do-it))

(global-linum-mode )

(provide 'init-linum)
