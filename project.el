;; Notes on projects:
;; The following code works on the current project, which is defined in project-directory.

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

(defun set-project (new-project-directory new-dfp-project)
  (interactive "sProject root: \nsdfp-project: ")
  (progn
    (setq project-directory new-project-directory)
    (setq dfp-project new-dfp-project)
    (set-project-file-cache)))

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
