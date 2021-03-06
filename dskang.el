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
;; (global-set-key "\C-x\C-m" 'execute-extended-command)

; Prefer backward-kill-word over Backspace
;; (global-set-key "\C-w" 'backward-kill-word)
;; (global-set-key "\C-x\C-k" 'kill-region)

; Set the comment character for asm-mode to '#'.
(setq asm-comment-char ?#)

; Indent using spaces instead of tabs
(setq indent-tabs-mode nil)

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

;; Auto-indentation
(dolist (hook '(c-mode-hook
                java-mode-hook
                html-mode-hook
                css-mode-hook
                php-mode-hook
                js-mode-hook))
  (add-hook hook '(lambda () (local-set-key "\C-m" 'reindent-then-newline-and-indent))))

;; Don't reindent current line in Python
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Color Theme
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path (concat user-specific-dir "/color-theme"))
(require 'color-theme)

;; Use the solarized-dark color theme
(add-to-list 'load-path (concat user-specific-dir "/color-theme-solarized"))
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Tramp
(setq tramp-default-method "ssh")

;; Use command key as meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)

;; Don't ask for confirmation upon creating new buffer or file
(setq confirm-nonexistent-file-or-buffer 'nil)

;; Enable ido everywhere
(ido-everywhere t)

;; Dedicated windows
;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  "Toggles whether the selected window is dedicated."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(global-set-key "\C-cd" 'toggle-current-window-dedication)

;; Apply shell environment to emacs
;; http://paste.lisp.org/display/111574
(defun env-line-to-cons (env-line)
  "Convert a string of the form \"VAR=VAL\" to a 
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
    (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "Launch /usr/bin/env or the equivalent from an interactive
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "$SHELL -lc")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

(defun setenv-from-cons (var-val)
  "Set an environment variable from a cons cell containing 
two strings, where the car is the variable name and cdr is 
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "Apply the environment reported by `/usr/bin/env' (or env-cmd) 
as launched by `$SHELL -lc' (or shell-cmd) to the current 
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

(setenv-from-shell-environment)
(setq exec-path (split-string (getenv "PATH") path-separator))

;; ERC settings
(require 'erc)
(setq erc-fill-column 72)
(setq erc-keywords '("_food_" "_fyi_"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "338" "353" "477"))
;; Set reasons for /part and /quit
(setq erc-part-reason 'erc-part-reason-various)
(setq erc-quit-reason 'erc-quit-reason-various)
(setq erc-part-reason-various-alist
      '(("version" erc-part-reason-normal)
        ("home" "Going home!")
        ("^$" "")))

;; Growl notifications for ERC
(defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
     message
     )))

(add-hook 'erc-text-matched-hook 'my-erc-hook)

;; File associations
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Start Emacs server
(server-start)

;; Split into as many vertical windows as possible
(defun smart-split ()
  "Split the frame into 100-column sub-windows, and make sure no window has
   fewer than 100 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
     100 columns."
    (if (> (window-width w) (* 2 101))
        (let ((w2 (split-window w 102 t)))
          (smart-split-helper w2))))
    (smart-split-helper nil))

;; Facebook JS indentation style
(setq-default js-indent-level 2)

(setq-default c-basic-offset 4)

;; Most files I'm working with use 4 space tabs...
;; (setq-default c-basic-offset 4)

;; Facebook Python indentation style
(setq-default python-indent 2)

;; Hiding ^M in files
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Disable vc-git
(setq vc-handled-backends nil)

;; Magit
(setq magit-omit-untracked-dir-contents t)
(global-set-key "\C-cg" 'magit-status)

;; Change Emacs font size
(custom-set-faces '(default ((t (:height 100)))))

;; Ignore prompt when killing a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Shell
(setq comint-prompt-read-only t)

;; Turn off visible bell
(setq visible-bell nil)

;; Don't automatically compile after saving in SCSS mode
(setq scss-compile-at-save nil)

;; Org-mode
(custom-set-variables '(org-replace-disputed-keys t)) ;; Avoid conflicts with windmove
(setq org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/courses/"))

;; Don't echo line in irb
(defun echo-false-comint ()
  (setq comint-process-echoes t))
(add-hook 'inf-ruby-mode-hook 'echo-false-comint)

;; Set lisp program to be used for SLIME
(add-to-list 'load-path (concat user-specific-dir "/slime"))
(setq inferior-lisp-program "clisp")
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;; Set up nXhtml mode
;; NOTE: nxhtml breaks evil-mode so only use when needed
;; (load (concat user-specific-dir "/nxhtml/autostart.el"))
(setq mumamo-background-colors nil)
;; Mumamo is making emacs 23.3 freak out:
;; http://stackoverflow.com/questions/5468952/how-do-i-hide-emacs-obsolete-variable-warnings
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require 'inf-haskell)

;; Assembler mode
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; CSS
(add-hook 'scss-mode-hook '(lambda () (setq css-indent-offset 2)))

;; Evil mode
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)
(add-to-list 'load-path (concat user-specific-dir "/evil"))
(require 'evil)
(evil-mode 1)

;; Bind kill-region since C-w is taken by window switching
(global-set-key "\C-x\C-k" 'kill-region)

;; Use 'jk' as ESC
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/684
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))
;; Show red box if in Emacs mode
(setq evil-emacs-state-cursor '("red" box))

;; Move using visual lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; General commands
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",q" 'kill-buffer) ; quit
(define-key evil-normal-state-map ",f" 'ido-find-file) ; find file
(define-key evil-normal-state-map ",b" 'ido-switch-buffer) ; show buffers
(define-key evil-normal-state-map ",o" 'org-agenda)  ; show org agenda

;; Lisp
(evil-define-key 'visual emacs-lisp-mode-map ",," 'eval-region)

;; Org-mode
(defun always-insert-item ()
     (interactive)
     (if (not (org-in-item-p))
       (insert "\n- ")
       (org-insert-item)))

(add-hook 'org-mode-hook
          (lambda ()
(evil-define-key 'normal org-mode-map "O" (lambda ()
                                            (interactive)
                                            (end-of-line)
                                            (org-insert-heading t)
                                            (evil-append nil)
                                            ))

(evil-define-key 'normal org-mode-map "o" (lambda ()
                                            (interactive)
                                            (end-of-line)
                                            (always-insert-item)
                                            (evil-append nil)
                                            ))

(evil-define-key 'normal org-mode-map "t" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (evil-append nil)
                     ))

(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-define-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style

(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

;; Org agenda - leave in Emacs mode but add j & k
(define-key org-agenda-mode-map "j" 'evil-next-line)
(define-key org-agenda-mode-map "k" 'evil-previous-line)
))
