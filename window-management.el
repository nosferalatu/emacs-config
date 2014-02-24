;; Window manipulation keys:
;; Use META-arrow to move cursor between windows (and frames)
;; Use CTRL-META-arrow to resize window
;; Use SHIFT-CTRL-META-arrow to resize window by larger amount
;; Use SHIFT-CTRL-arrow to rotate buffers around windows (but not frames, need to fix this)

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
