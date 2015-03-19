;; Configuration file. Either the .emacs or the emacs/site-lisp/site-start.el file should load this file.

;; Set elisp-directory to the directory where this config file is installed. We split exec-directory into its directories
;; and construct the path from that. We assume that Emacs is installed at the root of the drive (e.g. c:/emacs).
(let 
    ((dirs (split-string exec-directory "/")))
  (setq emacs-directory (concat (car dirs) "/" (nth '1 dirs) "/"))
  (setq elisp-directory (concat (car dirs) "/" (nth '1 dirs) "/elisp/")))

;; Set load-path to include all the directories in our elisp directory
(let
    ((default-directory elisp-directory))
  (add-to-list 'load-path elisp-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Common lisp
(require 'cl)

;; No splash screen. inhibit-splash-screen works if set in .emacs; initial-buffer-choice works if set when loading from the elisp directory.
(setq inhibit-splash-screen t)
(setq initial-buffer-choice 'none)

;; Set font
(set-frame-font "consolas-10" nil t)

;; Default placement and width/height of emacs window
(set-frame-position (selected-frame) 0 0)
(set-frame-width  (selected-frame) 154)
(set-frame-height (selected-frame) 59)

;; Disable the menu bar (File, Edit, Options...) and tool bar (icons) and scroll bar (not useful)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable fringe (without scrollbar, no reason for fringe)
(set-fringe-mode 0)

;; Change title bar to just "Emacs"
(setq frame-title-format '("Emacs"))

;; Smooth scrolling tips from http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(line-number-mode 1)
(column-number-mode 1)
(display-time)

(add-hook 'find-file-hooks 
	  '(lambda ()
	     (setq mode-line-buffer-identification 'buffer-file-truename)))

;; do not truncate long lines when window is split horizontally
(defconst truncate-partial-width-windows nil)

;; Theme
(add-to-list 'custom-theme-load-path elisp-directory) 
(add-to-list 'custom-theme-load-path (concat elisp-directory "thirdparty"))
(add-to-list 'custom-theme-load-path (concat elisp-directory "thirdparty/solarized"))
(setq solarized-broken-srgb t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'solarized-light t)
;;(load-theme 'deeper-blue t)
;;(load-theme 'monochrome t)
;;(load-theme 'bharadwaj t)
;;(load-theme 'planet t)
;;(load-theme 'github t)
(load-theme 'erosiond t)

;; Start Emacs server
;; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows
;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))
(or (server-running-p)
    (server-start))

;; Set up cc-mode
(require 'cc-mode)
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4
      indent-tabs-mode nil)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(setq default-tab-width 4)

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
                    (cons "\\.fxh$" 'c++-mode)       ; HLSL
                    (cons "\\.lua$" 'lua-mode))      ; Lua
              auto-mode-alist))

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
(global-set-key [f1] 'toggle-frame-fullscreen) 
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
;; F2 toggles bookmark at point, SHIFT-F2 clears all bookmarks, ctrl-pagedown/ctrl-pageup jumps next/previous
(require 'bm)
(setq bm-cycle-all-buffers t)
(setq bm-recenter nil)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(global-set-key (kbd "<f2>") 'bm-toggle)
(global-set-key (kbd "S-<f2>") 'bm-remove-all-all-buffers)
(global-set-key (kbd "C-<next>") 'bm-next)
(global-set-key (kbd "C-<prior>") 'bm-previous)
(global-set-key (kbd "C-c b m") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c b s") 'bm-show-all)
(global-set-key (kbd "C-c b a") 'bm-bookmark-annotate)
(global-set-key (kbd "C-c b c") 'bm-remove-all-all-buffers)
(global-set-key (kbd "C-c b <SPC>") 'bm-toggle)
(global-set-key (kbd "C-c b <up>") 'bm-previous)
(global-set-key (kbd "C-c b <left>") 'bm-previous)
(global-set-key (kbd "C-c b <down>") 'bm-next)
(global-set-key (kbd "C-c b <right>") 'bm-next)

;; Don't make damn ~ backup files
(setq backup-inhibited "true")

;; But on each save, create a backup file in e.g. c:/emacs/backups
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
(setq backup-each-save-mirror-location (concat emacs-directory "backups"))

;; Load new tags file without prompting user to 
(setq tags-revert-without-query 1)

;; Inhibit warning when loading large files
(setq large-file-warning-threshold nil)

;; For PS2 VU Asm mode
;; (require 'vuasm-mode)

;; Remap C-x C-b to use electric buffer
(global-unset-key [?\C-x ?\C-b]) 
(global-set-key [?\C-x ?\C-b] 'electric-buffer-list) 

;; Default command to use for M-x compile
(setq compile-command "make.bat SfNoamMicrocodeRender")
(setq compilation-scroll-output t)

(defun yes-or-no-p (prompt)
  "replace tedious yes/no+enter with y/n keypress"
  (ding t)
  (y-or-n-p prompt))

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(add-hook 'c-mode-common-hook 
	  (function (lambda ()
		      (setq c-recognize-knr-p nil))))

; Display full path name if appropriate on mode line.
; Blink Emacs rather than ring the bell
;; (setq visible-bell t)

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

(load "window-management.el")

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
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n-> " "")))

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

(load "p4.el")

;; Ctrl-up/down scrolls the window but leaves the cursor in the same place on the screen
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)

;; jpeg is bound by default; add jpg, JPG, JPEG
(setq dynamic-library-alist (cons '(jpg "jpeg62.dll") dynamic-library-alist))
(setq dynamic-library-alist (cons '(jpeg "jpeg62.dll") dynamic-library-alist))
(setq dynamic-library-alist (cons '(JPG "jpeg62.dll") dynamic-library-alist))
(setq dynamic-library-alist (cons '(JPEG "jpeg62.dll") dynamic-library-alist))

;; set up python-mode so that C-c C-c behaves like C-u C-c C-c (so it executes if __name__ == '__main__')
;; also, pop up the python shell buffer
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c C-c")
                       (lambda () (interactive)
                         (python-shell-send-file buffer-file-name)
                         (python-shell-switch-to-shell)))))

;; Set up PDB debugging. pdb.py isn't in my path, and Emacs PDB mode on Windows needs -u for unbuffered output
(setq gud-pdb-command-name "python -u -m pdb")

;; Set up Jedi for Python autocomplete
;; The first time you run Emacs Jedi on a machine, you may need to run M-x jedi:install-server
;; Also, you'll need a recent version of virtualenv (you can upgrade via 'pip install virtualenv --upgrade')
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;; For comint modes (command shell, Python shell, etc) put the cursor at the bottom of the buffer on output
;; As soon as you type any input, put it at the bottom of the comint buffer
;; Set the prompt as read only, so pressing delete doesn't erase the prompt
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-prompt-read-only t)

;; Turn off shell-dirtrack-mode, which tries to detect the commands cd,pushp, and popd but fails to detect "cd.."
;; which is a valid command in Windows cmd.exe. Instead, just look at the command line prompt to find the
;; current directory.
(add-hook 'shell-mode-hook
        '(lambda ()
            (shell-dirtrack-mode nil)
            (add-hook 'comint-preoutput-filter-functions 'shell-sync-dir-with-prompt nil t)))

(defun shell-sync-dir-with-prompt (string)
  (if (string-match "^\\([a-zA-Z]:.*\\)>" string)
      (let* ((str string)
             (cwd (match-string 1 str)))
        (setq default-directory
              (if (string-equal "/" (substring cwd -1))
                  cwd
                (setq cwd (concat cwd "/"))))
        string)
    string))

;; Shell tab complete uses backslash
(setq comint-completion-addsuffix (quote ("\\" . " ")))

;; M-s starts a new shell with a unique name 
(global-set-key (kbd "M-s") '(lambda () (interactive) (shell (generate-new-buffer (generate-new-buffer-name "*shell*")))))

;; set up melpa
(require 'package)
(setq package-user-dir (concat elisp-directory "thirdparty/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands) ;; shows commands relevant to active major mode

;; Instead of playing the alarm bell sound, flash the modeline when there are errors
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line) 
   (run-with-timer 0.05 nil 'invert-face 'mode-line))
 
(setq visible-bell nil)
(setq ring-bell-function 'my-terminal-visible-bell)

;; Final steps: Load local.el if it exists, and then create and switch to buffer *default*.
;; Local.el can contain any computer-specific configuration (it's in the .gitignore list).
(load "local.el" t)
