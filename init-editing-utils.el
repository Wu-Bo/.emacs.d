(require 'move-text)
(move-text-default-bindings)

(define-key global-map (kbd "RET") 'newline-and-indent)

;;----------------------------------------------------------------------------
;; Zap *up* to char is a more sensible default
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)


;; ---------------------------------------------------------------------------
;; make a duplicate line
;;----------------------------------------------------------------------------
(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(global-set-key (kbd "C-c p") 'duplicate-line)

;; --------------------------------------------------------------------------
;; transpose-selections from primary sel to secondary sel
;; --------------------------------------------------------------------------

(defun transpose-selections ()
  "Transpose the content of the primary region and of the secondary."
  (interactive)
  (let ( (osecondary  (x-get-selection 'SECONDARY)) )
    (unless (and osecondary (overlayp mouse-secondary-overlay))
      (error "No secondary selection"))
    (unless (eq (current-buffer) (overlay-buffer mouse-secondary-overlay))
      (error "Primary selection and secondary selection should be in same buffer."))
    (let* ( (pri-start (region-beginning))
            (pri-end   (region-end))
            (pri-content (buffer-substring pri-start pri-end))
            (sec-start (overlay-start mouse-secondary-overlay))
            (sec-end   (overlay-end mouse-secondary-overlay))
            (sec-content (buffer-substring sec-start sec-end)) )
      ;;(message "swap `%s' with `%s'." pri-content sec-content)
      ;;FIXME: ugly code. any good idea?
      (if (> sec-start pri-start)
          (progn
            ;; move primary's content to secondary's location
            (delete-region sec-start sec-end)
            (goto-char sec-start)
            (insert-string pri-content)

            ;; move secondary's to primary
            (delete-region pri-start pri-end)
            (goto-char pri-start)
            (insert-string sec-content))
        (progn
            ;; move secondary's to primary          
            (delete-region pri-start pri-end)
            (goto-char pri-start)
            (insert-string sec-content))
            
            ;; move primary's content to secondary's location
            (delete-region sec-start sec-end)
            (goto-char sec-start)
            (insert-string pri-content)          
      ))))
      
;; swap the primay and secondary region
;;(global-set-key (kbd "C-x t") 'transpose-selections)

;;-------------------------------------------------------------
;;  mark a line , then move the point to the beginning
;;-------------------------------------------------------------

(defun mark-line ()
  "Mark one whole line, similar to `mark-paragraph'."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

;;-------------------------------------------------------------
;; insert a line with the char that given
;;-------------------------------------------------------------

(defun underline-line-with (char)
  "Insert some char below at current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

;; ------------------------------------------------------------
;; match paren from beg to end or from end to beg
;; ------------------------------------------------------------

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t (self-insert-command (or arg 1)))))

;;------------------------------------------------------------
;;  more useful function
;;------------------------------------------------------------
(require 'thing-edit)

;; -----------------------------------------------------------
;; move to nextline at any point in the line
;; -----------------------------------------------------------
(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

  (global-set-key [(control o)] 'vi-open-next-line)

;; -----------------------------------------------------------
;; Find duplicate lines and keep only the first occurrence
;; -----------------------------------------------------------

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(global-set-key (kbd "C-c C-d")  'uniquify-all-lines-region)
(global-set-key (kbd "C-c M-d")  'uniquify-all-lines-buffer)

;; -----------------------------------------------------------
;; smart beg and end
;; -----------------------------------------------------------


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to beginning-of-line ,if point was already at that position,
  move point to first non-whitespace character. "
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (and (= oldpos (point))
         (back-to-indentation) )))

(defun smart-end-of-line()
  "Move point to first non-whitespace character or end-of-line.
Move point to end-of-line ,if point was already at that position,
  move point to first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (when (re-search-forward "[ \t]*$" (point-at-eol) t)
      (goto-char (match-beginning 0)))
    (when (= oldpos (point))
      (end-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)

;; ----------------------------------------------------------
;; operations between pattern
;; ----------------------------------------------------------

(defun mark-between-pattern (within pattern)
  "Mark the region between the previous and the next occurrence of the PATTERN.
If WITHIN is non-nil, only text within marked, otherwise leading and ending occurrence of
the PATTERN is included."
  (interactive "P\nsPattern: ")
  (let ( (start (progn (search-backward pattern) (point))) )
    (if within (forward-char (length pattern)))
    (push-mark (point) 'nomsg 'activate)
    (forward-char 1)
    (search-forward pattern)
    (if within (backward-char 1))))

(defun mark-between-char (within char)
  "Mark the region between a pair of CHAR."
  (interactive (list current-prefix-arg
					 (format "%c" last-command-event)))
  (mark-between-pattern within char))

(defun copy-between-pattern (within pattern)
  (interactive "*P\nsPattern: ")
  (mark-between within pattern)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-between-char (within char)
  "Copy the region between a pair of CHAR."
  (interactive (list current-prefix-arg
                     (format "%c" last-command-event)))
  (mark-between-pattern within char)
  (copy-region-as-kill (region-beginning) (region-end)))

;; -------------------------------------------------------------
;; useful config for compile gdb shell
;; -------------------------------------------------------------

;; Helper for compilation. Close the compilation window if
;; there was no error at all. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)


;;shell,gdb退出后，自动关闭该buffer  
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
	(when process
	  (set-process-sentinel process
							(lambda (proc change)
							  (when (string-match "\\(finished\\|exited\\)" change)
								(kill-buffer (process-buffer proc))))))))

;; 退出gdb的时候关闭gdb对应的buffer
(add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)
;; 退出term的时候关闭term对应的buffer
(add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-shell-command-exit)


;; ---------------------------------------------------------------------------
;; 编程相关
;; ---------------------------------------------------------------------------

;; 拷贝代码自动格式化
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
	  (and (not current-prefix-arg)
		   (member major-mode
				   '(emacs-lisp-mode
					 lisp-mode
					 clojure-mode
					 scheme-mode
					 haskell-mode
					 ruby-mode
					 rspec-mode
					 python-mode
					 c-mode
					 c++-mode
					 objc-mode
					 latex-mode
					 js-mode
					 plain-tex-mode))
		   (let ((mark-even-if-inactive transient-mark-mode))
			 (indent-region (region-beginning) (region-end) nil))))))


;; c  代码风格
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)
(setq c-basic-offset 8 )
(setq default-tab-width 8)

;;c-mode或cc-mode下缩进只有4格
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "Stroustrup")))


(require 'browse-mark-ring)


(provide 'init-editing-utils)
