; Turn on syntax highlighting.
(global-font-lock-mode t)
;(setq font-lock-maximum-decoration t)

; Display the column number in the status bar.
(setq column-number-mode t)

; Highlight the region bounded by the mark and the point.
(setq transient-mark-mode t)

; Highlight matching parentheses, braces, and brackets.
(show-paren-mode t)

; Enable the Ctrl-x Ctrl-m sequence for Alt-x
(global-set-key "\C-x\C-m" 'execute-extended-command)

; Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

; Set the comment character for asm-mode to '#'.
(setq asm-comment-char ?#)

; Indent using spaces instead of tabs in c-mode and asm-mode.
(defun my-c-mode-common-hook ()
   (setq indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(defun my-asm-mode-common-hook ()
   (setq indent-tabs-mode nil))
(add-hook 'asm-mode-hook 'my-asm-mode-common-hook)

; Make the Control-n and Control-p keys (and the down arrow and up
; arrow keys) scroll the current window one line at a time instead
; of one-half screen at a time.
(setq scroll-step 1)

; Set the indentation style for C code.
(setq c-default-style "ellemtel")
; (setq c-default-style "cc-mode")
; (setq c-default-style "gnu")
; (setq c-default-style "k&r")
; (setq c-default-style "bsd")
; (setq c-default-style "stroustrup")

; Bind the "Control-x p" key sequence to the function "indent-all".
; The "indent-all" function indents all lines of the C program in the 
; current buffer according to the selected indentation style.
;; (defun indent-all()
;;    (interactive)
;;    (save-excursion
;;       (let ()
;;          (goto-char (point-min))
;;          (while (< (point) (point-max))
;;             (c-indent-command)
;;             (next-line 1)))))
;; (global-set-key "\C-xp" 'indent-all)

;; Electric Pairs
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key python-mode-map "\"" 'electric-pair)
;;             (define-key python-mode-map "\'" 'electric-pair)
;;             (define-key python-mode-map "(" 'electric-pair)
;;             (define-key python-mode-map "[" 'electric-pair)
;;             (define-key python-mode-map "{" 'electric-pair)))
;; (defun electric-pair ()
;;   "Insert character pair without surrounding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;      (insert-pair)))

;; Auto-indentation for Python files
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Auto-indentation for Java files
(add-hook 'java-mode-hook '(lambda () (define-key java-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Auto-indentation for HTML files
(add-hook 'html-mode-hook '(lambda () (define-key html-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Auto-indentation for PHP files
(add-hook 'php-mode-hook '(lambda () (define-key php-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Color Theme
(require 'color-theme)
(color-theme-zenburn)

;; Tramp
(setq tramp-default-method "ssh")

;; Use command key as meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)
