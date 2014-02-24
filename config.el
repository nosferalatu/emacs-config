;; Configuration file. Either the .emacs or the site-list file should contain just the following:
;;  (load "config.el")

 ;; Double Fine lisp
;;(add-to-list 'load-path "d:/dfp-seed/main/tools/lisp"

;; DOUBLE FINE!
;;(require 'p4)

;;(load "lua-mode")

;; No splash screen. inhibit-splash-screen works if set in .emacs; initial-buffer-choice works if set when loading site-lisp.
(setq inhibit-splash-screen t)
(setq initial-buffer-choice 'none)

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

;; NOVA
(defun luagrep (prompt)
  (interactive "sGrep for: ")
  (if (get-buffer "*grep*") ; If old grep window exists
      (progn
        (delete-windows-on (get-buffer "*grep*")) ; delete the grep windows
        (kill-buffer "*grep*") ; and kill the buffers
        )
    )
  (let ((luagrep-directory '"c:/asobi/target/common/data/hello_double_fine"))
        (compilation-start 
         (format "grep -n -r --include=*.cpp --include=*.c --include=*.h --include=*.lua --include=*.py --include=*.fx --include=*.fxh \%s \%s." prompt luagrep-directory)
         'grep-mode))
)

;; Notes on projects:
;; The following code works on the current project, which is defined in project-directory.
;; This can be set automatically in .dir-locals.el. 

;; Given the project-file-cache variable, create a list of all the directories named project-directory-cache.
(defun set-project-directory-cache ()
  (setq project-directory-cache
        (let (dirs)
          (dolist (file project-file-cache)
            (push (file-name-directory file) dirs))
          (delete-dups dirs))))

;; Build the tags file for the project at project-directory
(defun create-project-tags ()
  (progn
    (message (concat "Building tags for project " project-directory "..."))
    (cd project-directory)
    (call-process "cmd.exe" nil "*output*" nil "/c dir /s /b *.h *.c *.cpp *.lua *.py > tagfiles.txt")
    (call-process "cmd.exe" nil "*output*" nil "/c c:/emacs/bin/etags.exe - < tagfiles.txt")
    (call-process "cmd.exe" nil "*output*" nil "/c erase tagfiles.txt")
    (message "Loading tags for project " project-directory "...")
    (visit-tags-table "tags")))

;; Caches the files in the current project (starting at
;; project-directory) into the variable project-file-cache. That
;; variable is used by the my-ido-project-files function.
(defun set-project-file-cache ()
  "Cache the files in the project for ido fast find"
  (interactive)
  ;; get project files
  (progn
    (cd project-directory)

    ;; (create-project-tags)

    (message (concat "Caching files for project " project-directory "..."))
    (setq project-file-cache
          (split-string 
           (shell-command-to-string 
            "cmd.exe /c dir /s /b *.cpp *.h *.lua *.fx *.fxh *.py") "\n"))

    ;; The above code generates a nil as the last entry, so remove it
    (delete "" project-file-cache)

    (set-project-directory-cache)

    (setq cc-search-directories '("." "../src" "../Inc" "../include" "../../src" "../../inc" "../../include" project-directory-cache))

    (message (concat "Done caching files for " project-directory))
  )
)

(defun set-project-massive-chalice () (interactive) (progn (setq project-directory "c:/dfp-mc/") (setq dfp-project "c:/dfp-mc/mc/") (set-project-file-cache)))
(defun set-project-massive-chalice-branch () (interactive) (progn (setq project-directory "c:/dfp-mc-sc/") (setq dfp-project "c:/dfp-mc/mc/") (set-project-file-cache)))
(defun set-project-seed () (interactive) (progn (setq project-directory "d:/dfp-seed/") (setq dfp-project "d:/dfp-seed/seed/") (set-project-file-cache)))
(defun set-project-steed () (interactive) (progn (setq project-directory "c:/dfp-afsteed/") (setq dfp-project "c:/dfp-afsteed/afsteed/") (set-project-file-cache)))
(defun set-project-mnemonic () (interactive) (progn (setq project-directory "c:/dfp-afmnem/") (setq dfp-project "c:/dfp-afmnem/afmnem/") (set-project-file-cache)))
(defun set-project-buds () (interactive) (progn (setq project-directory "c:/dfp-afbuds/") (setq dfp-project "c:/dfp-afbuds/afbuds/") (set-project-file-cache)))
(defun set-project-asobi () (interactive) (progn (setq project-directory "c:/asobi/") (setq dfp-project "") (set-project-file-cache)))

(defun codegrep (prompt)
 (interactive (list (read-string (format "Grep code in \%s for: " project-directory))))
;; (if (get-buffer "*grep*") ; If old grep window exists
;;     (progn 
;;       ;;(delete-windows-on (get-buffer "*grep*")) ; delete the grep windows
;;       ;;(kill-buffer "*grep*") ; and kill the buffers
;;       )
;;   )
 (let ((codegrep-directory
       (if (boundp 'project-directory)
           project-directory
           default-directory)))
       (compilation-start 
        (format "grep -n -r --include=*.cpp --include=*.c --include=*.h --include=*.lua --include=*.py --include=*.fx --include=*.fxh \%s \%s." prompt codegrep-directory)
        'grep-mode))
 )
(global-set-key (kbd "C-S-s") 'codegrep)

;; Always open *grep* windows in the current window; inhibit creating a new one
;; (add-to-list 'same-window-buffer-names "*grep*")

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

;; Use Ido to find files. The list of files is the project-file-cache variable.
(defun my-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let ()
  ;; populate hash table (display repr => path)
  (setq tbl (make-hash-table :test 'equal))
  (let (ido-list)
  (mapc (lambda (path)
      ;; format path for display in ido list
      (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
      ;; strip project root
      (setq key (replace-regexp-in-string project-directory "" key))
      ;; remove trailing | or /
      (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
      (puthash key path tbl)
      (push key ido-list)
      )
    project-file-cache
    )
  (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))
(global-set-key (kbd "C-S-f") 'my-ido-project-files)

(defun foreslash-to-backslash (str) (replace-regexp-in-string "/" "\\" str t t))
(defun backslash-to-foreslash (str) (replace-regexp-in-string "\\\\" "/" str t t))

;; Compile a Double Fine shader.
;; Note the hard-coded MC in the path. That should be specified in a new project specific variable.
(defun compile-shader ()
  "Compile a Double Fine shader"
  (interactive)
  (progn
    ;; Get shader files
    (setq shader-files
          (split-string 
           (shell-command-to-string 
            (concat "cmd.exe /c dir /s /b " (foreslash-to-backslash(concat dfp-project "Code\\ShaderLibrary\\Src\\*.fx"))))
           "\n"))

    ;; Ask which shader to compile
    (setq shader-file (ido-completing-read "shader: " shader-files))

    ;; Compile
    (setq command (concat dfp-project "win/bin/ShaderCompiler_win.exe " shader-file))
    (setq compilation-scroll-output 'true)
    (compilation-start command)
))

;; Redo the last compile-shader command.
(defun recompile-shader ()
  "Compile a Double Fine shader"
  (interactive)
  (progn
    (setq command (concat dfp-project "win/bin/ShaderCompiler_win.exe " shader-file))
    (setq compilation-scroll-output 'true)
    (compilation-start command)
))

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

;; How to match files
(setq other-file-alist
      '((".c"    . ".h")
        (".cpp"  . ".h")
        (".h"    . ".cpp")
        (".fx"   . ".fxh")
        (".fxh"  . ".fx")
))

;; Given a filename, return the matching filename using the other-file-alist.
;; Returns nil if the filename can't be matched.
(defun get-file-matching-extension (file)
  (let (extension matching-extension matching-file)
    (setq extension (file-name-extension file))
    (when (setq matching-extension (cdr (assoc (concat "." extension) other-file-alist)))
      (setq matching-file (concat (file-name-sans-extension file) matching-extension)))
))

;; Returns the matching filename of the current buffer, or if the cursor is on a #include line, the #include path.
;; Returns nil if the filename can't be matched.
(defun get-include-path-or-matching-filename ()
  (let (line file extension matching-extension)
    (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (if (string-match "^\#\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]" line)
        (setq file (match-string 2 line))
      (setq file (get-file-matching-extension(file-name-nondirectory buffer-file-name))))
    file
    ))

;; Given a filename (possibly including a partial path), return a list of all the project files with that name and partial path.
;; "partial path" means DFGraphics/Inc/ in the filename DFGraphics/Inc/GraphicsManager.h.
(defun find-in-project (file)
  (when (not (null file))
    (let ((result '()))
      (if (not (equal (substring file 0 1) "/"))
          (setq file (concat "/" file)))
      (dolist (f project-file-cache)
        (if (string-match file (backslash-to-foreslash f))
            (add-to-list 'result f)))
      result))
)

;; Find the current buffer's matching file using the other-file-alist.
;; If the current list is a #include, find the file referenced by the #include.
(defun find-matching-file()
  (interactive)
  (let 
      ((matching-filename) (matches))
      (setq matching-filename (get-include-path-or-matching-filename))
      (if (not (null matching-filename))
          (progn
            (setq matches (find-in-project matching-filename))
            (cond
             ((or (null matches) (= 0 (length matches)))
              (message (concat "Cannot find " matching-filename " in project.")))
             ((= 1 (length matches))
              (find-file (car matches)))
             (t 
              (find-file (ido-completing-read "Matching files: " matches)))))
          (message (concat "Unknown extension '." (file-name-extension buffer-file-name) "'")))
  ))

(global-set-key [f3] 'find-matching-file) 

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;
(defun spotify-pause-play-toggle()
  (interactive)
  (save-excursion
    (find-file "c:/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 917504)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/spotifycommand.py")))

(global-set-key (kbd "C-M-<pause>") 'spotify-pause-play-toggle)

;;
(defun spotify-next()
  (interactive)
  (save-excursion
    (find-file "c:/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 720896)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/spotifycommand.py")))

;;
(defun spotify-prev()
  (interactive)
  (save-excursion
    (find-file "c:/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 786432)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/spotifycommand.py")))

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
