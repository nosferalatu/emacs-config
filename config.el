;; Configuration file. Either the .emacs or the site-list file should contain just the following:
;;  (load "config.el")

 ;; Double Fine lisp
;;(add-to-list 'load-path "d:/dfp-seed/main/tools/lisp"

;; DOUBLE FINE!
;;(require 'p4)

;; No splash screen. inhibit-splash-screen works if set in .emacs; initial-buffer-choice works if set when loading site-lisp.
(setq inhibit-splash-screen t)
(setq initial-buffer-choice 'none)

;; Start Emacs server
;; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows
;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))
(server-start)

(require 'cl)

(add-to-list 'custom-theme-load-path "c:/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "c:/emacs/site-lisp/thirdparty")
(add-to-list 'custom-theme-load-path "c:/emacs/site-lisp/thirdparty/solarized")
(setq solarized-broken-srgb t)
;;(load-theme 'zenburn t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'solarized-light t)
(load-theme 'deeper-blue t)

;; Set font
(set-default-font "Consolas-9")

;; Default placement and width/height of emacs window
(set-frame-position (selected-frame) 0 0)
;;(set-frame-size (selected-frame) 5000 20)

;; Disable the menu bar (File, Edit, Options...) and tool bar (icons) and scroll bar (not useful)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable fringe (without scrollbar, no reason for fringe)
(set-fringe-mode 0)

(set-frame-width  (selected-frame) 154)
(set-frame-height (selected-frame) 59)

(require 'w32-fullscreen)

;; Set up cc-mode
(require 'cc-mode)
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4
      indent-tabs-mode nil)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(defun my-text-mode-setup nil
  (auto-fill-mode 1)
  (defconst fill-column 120))
(setq text-mode-hook 'my-text-mode-setup)

(load "lua-mode")

(setq auto-mode-alist
      (append (list (cons "\\.3$" 'nroff-mode)
                    (cons "\\.txt$" 'text-mode)
                    (cons "\\README" 'text-mode)
                    (cons "\\TODO" 'text-mode)
                    (cons "\\.C$" 'c++-mode)
                    (cons "\\.CPP$" 'c++-mode)
                    (cons "\\.p$" 'pascal-mode)
                    (cons "\\.H$" 'c++-mode)
                    (cons "\\.1$" 'nroff-mode)
                    (cons "\\.me$" 'nroff-mode)
                    (cons "\\.vsm$" 'vuasm-mode)     ; PS2 ucode
                    (cons "\\.vcl$" 'vuasm-mode)     ; PS2 ucode
                    (cons "\\.dsm$" 'vuasm-mode)     ; PS2 ucode
                    (cons "\\.vsh$" 'asm-mode)       ; XBox vertex shader
                    (cons "\\.psh$" 'asm-mode)       ; XBox pixel shader
                    (cons "\\.fx$" 'c++-mode)        ; HLSL
                    (cons "\\.lua$" 'lua-mode))      ; Lua
              auto-mode-alist))

;; do not truncate long lines when window is split horizontally
(defconst truncate-partial-width-windows nil)

(defun repeat-commands nil
  (global-set-key "\e[193z" 'previous-cmplex-command)
  (repeat-complex-command)
  (global-set-key "\e[193z" 'repeat-commands))

(defun dos2unix nil
   (interactive)
   (goto-line 0)
   (replace-regexp "$" ""))
(provide 'dos2unix)

(global-set-key [f21]         'what-line)             ;R1
(global-set-key [f22]         'goto-line)             ;R2
(global-set-key [f23]         'what-cursor-position)  ;R3
(global-set-key [f24]         'other-window)          ;R4
(global-set-key [f25]         'comment-out)           ;R5
(global-set-key [f26]         'uncomment-out)         ;R6
(global-set-key [f27]         'beginning-of-buffer)   ;R7
(global-set-key [f29]         'scroll-down)           ;R9
(global-set-key [f31]         'recenter)              ;R11
(global-set-key [f33]         'end-of-buffer)         ;R13
(global-set-key [f35]         'scroll-up)             ;R15
(global-set-key [kp-subtract] 'shrink-window)         ;-
(global-set-key [kp-add]      'enlarge-window)        ;+
(global-set-key [kp-insert]   'overwrite-mode)        ;Ins
(global-set-key [kp-enter]    'yow)                   ;Enter
 
(global-set-key [f13]         'apropos)               ;L3
(global-set-key [f14]         'undo)                  ;L4
(global-set-key [f16]         'copy-region-as-kill)   ;L6
(global-set-key [f17]         'find-file)             ;L9
(global-set-key [f18]         'yank)                  ;L8
(global-set-key [f19]         'find-file)             ;L9
(global-set-key [f20]         'kill-region)           ;L10
(global-set-key [help]        'help)                  ;Help

;;(defvar diff-switches nil)
(defvar diff-switches " -h ")

(global-set-key [home] 'beginning-of-line) 
(global-set-key [end] 'end-of-line) 
(global-set-key [f1] 'w32-fullscreen) 
(global-set-key [f2] 'find-matching-file) 
(global-set-key [f3] 'split-window-horizontally) 
(global-set-key [f4] 'delete-window) 
(global-set-key [f5] '[?\C-u ?\M-.])
(global-set-key [f6] '[?\C-u -?\M-.])
(global-set-key [f7] 'recompile)
(global-set-key [f9] 'start-kbd-macro)
(global-set-key [f10] 'end-kbd-macro)
(global-set-key [f11] 'call-last-kbd-macro) 
;;(global-set-key [f12] 'speedbar) 
(global-unset-key [?\M-g]) 
(global-set-key [?\M-g] 'goto-line) 
(global-unset-key [?\M-u]) 
(global-set-key [?\M-u] 'undo) 
(global-set-key [\e] 'keyboard-quit) 
(global-set-key [?\C-z] 'undo)

(transient-mark-mode t) 

(setq-default indent-tabs-mode nil)

;; NT-emacs assumes a Windows command shell, which you change
;; here.
;;
;; (setq process-coding-system-alist '(("bash" . undecided-unix)))
;; (setq w32-quote-process-args ?\")
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name) 
;; (setq explicit-shell-file-name shell-file-name) 
;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; Load the font-lock package.
(require 'font-lock)
;; Maximum colors
(setq font-lock-maximum-decoration t)
;; Turn on font-lock in all modes that support it
(global-font-lock-mode t)

;; Bookmarks
(require 'bm)
(setq bm-cycle-all-buffers t)
(setq bm-recenter t)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(global-set-key (kbd "C-c b m") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c b s") 'bm-show-all)
(global-set-key (kbd "C-c b a") 'bm-bookmark-annotate)
(global-set-key (kbd "C-c b c") 'bm-remove-all-all-buffers)
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "S-<f2>") 'bm-previous)

;; Don't make damn ~ backup files
(setq backup-inhibited "true")

;; Load new tags file without prompting user to 
(setq tags-revert-without-query 1)

;; Inhibit warning when loading large files
(setq large-file-warning-threshold nil)

;; Smooth scrolling tips from http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; For PS2 VU Asm mode
;; (require 'vuasm-mode)

;; Remap C-x C-b to use electric buffer
(global-unset-key [?\C-x ?\C-b]) 
(global-set-key [?\C-x ?\C-b] 'electric-buffer-list) 

;; Default command to use for M-x compile
(setq compile-command "make.bat SfNoamMicrocodeRender")

(defun yes-or-no-p (prompt)
  "replace tedious yes/no+enter with y/n keypress"
  (ding t)
  (y-or-n-p prompt))

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(add-hook 'c-mode-common-hook 
	  (function (lambda ()
		      (setq c-recognize-knr-p nil))))

; If the time is not displayed on the modeline, then the entire .emacs
; file was not read
(display-time)

; Display full path name if appropriate on mode line.
(add-hook 'find-file-hooks 
	  '(lambda ()
	     (setq mode-line-buffer-identification 'buffer-file-truename)))

; Blink Emacs rather than ring the bell
;; (setq visible-bell t)

(line-number-mode 1)
;;(global-linum-mode 1)
(column-number-mode 1)

(setq cc-other-file-alist
  '(("\\.cpp$" (".h"))
   ("\\.h$" (".cpp"))
   ("\\.fx$" (".fxh" ".h"))
   ("\\.fxh$" (".fx"))
   ))

(require 'thingatpt)

(setq split-height-threshold 100)
(setq split-width-threshold nil)
(setq grep-scroll-output t)

;; Window manipulation keys:
;; Use META-arrow to move cursor between windows (and frames)
;; Use SHIFT-CTRL-arrow to rotate buffers around windows (but not frames, need to fix this)
;; Use CTRL-META-arrow to resize window

;; Set up meta-arrow
;; Use framemove to move across frames (windmove only moves within current frame)
(windmove-default-keybindings 'meta)
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; Set up ctrl-meta-arrow to resize window (code from http://www.emacswiki.org/emacs/WindowResize)
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the middle"
  (let* ((win-edges (window-edges))
         (this-window-y-min (nth 1 win-edges))
         (this-window-y-max (nth 3 win-edges))
         (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the middle"
  (let* ((win-edges (window-edges))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (message "win-resize-minimize-vert")
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-enlarge-horiz-large ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -10))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 10))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -10))
   (t (message "nil"))))

(defun win-resize-minimize-horiz-large ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 10))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -10))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 10))
   (t (message "nil"))))

(defun win-resize-enlarge-vert-large ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -10))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 10))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 10))))

(defun win-resize-minimize-vert-large ()
  (interactive)
  (message "win-resize-minimize-vert")
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 10))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -10))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -10))))

(global-set-key (kbd "C-M-<down>") 'win-resize-minimize-horiz)
(global-set-key (kbd "C-M-<up>") 'win-resize-enlarge-horiz)
(global-set-key (kbd "C-M-<left>") 'win-resize-enlarge-vert)
(global-set-key (kbd "C-M-<right>") 'win-resize-minimize-vert)

(global-set-key (kbd "C-S-M-<down>") 'win-resize-minimize-horiz-large)
(global-set-key (kbd "C-S-M-<up>") 'win-resize-enlarge-horiz-large)
(global-set-key (kbd "C-S-M-<left>") 'win-resize-enlarge-vert-large)
(global-set-key (kbd "C-S-M-<right>") 'win-resize-minimize-vert-large)

;; Set up shift-ctrl-arrow to rotate buffers around windows
(require 'buffer-move)
(global-set-key (kbd "C-S-<up>")     'buf-move-up)
(global-set-key (kbd "C-S-<down>")   'buf-move-down)
(global-set-key (kbd "C-S-<left>")   'buf-move-left)
(global-set-key (kbd "C-S-<right>")  'buf-move-right)

;; Set TAB to use hippie-expand, unless it should indent
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
    (indent-according-to-mode)))
(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))
(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'c++-mode-hook        'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'lua-mode-hook 'my-tab-fix)

(defun next-error-recenter ()
  (interactive)
  (progn
    (next-error)
    (recenter)))
(defun previous-error-recenter ()
  (interactive)
  (progn
    (previous-error)
    (recenter)))
(global-set-key (kbd "M-n") 'next-error-recenter)
(global-set-key (kbd "M-p") 'previous-error-recenter)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(load "project.el")

(defun copy-file-name-to-clipboard ()
  (interactive)
  (w32-set-clipboard-data buffer-file-name))

;; ctrl-c left and ctrl-c right will undo/redo window configurations
(winner-mode 1)

;; Google something using the default browser
(defun google (query-string)
  (interactive "sGoogle: ")
  (browse-url
   (concat "http://www.google.com/#q=" query-string)))

;; Change title bar to just "Emacs"
(setq frame-title-format '("Emacs"))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case 
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(load "spotify.el")

;; Are you really sure you want to exit?
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
        (save-buffers-kill-emacs)
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(require 'undo-tree)
(global-undo-tree-mode)
;; Always split vertically for undo-tree visualizer
(defadvice undo-tree-visualize (around undo-tree-split-vertically activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)  ;; M-y invokes kill ring browser

;; Create a new buffer named *default* and switch to it
(generate-new-buffer "*default*")
(switch-to-buffer "*default*")
