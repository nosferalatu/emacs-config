;; Notes on projects:
;; The following code works on the current project, which is defined in project-directory.

;; Global vars:
;; project: name of the project (currently same as its root)
;; project-file-extensions: list of filename extensions to include in projects

;; The global var project has the following properties:
;; root: the root of the project
;; filecache: cache of the filenames in the project
;; dfp-project: double fine root

;; TODO:
;; the global var 'project' now holds properties. Its value is its name but that is unused.
;; turn project into project-list
;; use the buffer's filename's root to figure out which project in project-list to use
;; add way to switch to project (maybe open empty buffer but with the project set somehow?)
;; add way to recache files for a specific project
;; add way to add project to list of projects
;; add way to delete a project from the list?
;; need a nice way to specify search filters
;; add way to search a different project
;; automatically find shadercompiler and eliminate dfp-project

(setq project nil)

(setq project-file-extensions '(".cpp" ".c" ".h" ".lua" ".fx" ".fxh" ".py"))

;; This returns a concatenated string of the project-file-extensions variable
;; e.g. ".cpp .c .h ..."
(defun project-file-extensions-string ()
  ((lambda (l)
     (let ((str (car l))
           (list (cdr l)))
       (while (car list)
         (setq str (concat str " " (car list)))
         (setq list (cdr list)))
       str))
   project-file-extensions))

;; This returns a concatenated string of the project-file-extensions variable
;; where each entry is prepended with *, e.g. "*.cpp *.c *.h ..."
(defun project-file-extensions-string-wildcards ()
  (replace-regexp-in-string "\\." "\*\." (project-file-extensions-string)))

;; This returns a concatenated string of the project-file-extensions variable
;; where each entry is prepended with --include=*, e.g. "--include=*.cpp --include=*.c --include*.h ..."
;; Used to build arguments for grep
(defun project-file-extensions-string-include-wildcards ()
  (replace-regexp-in-string "\\." "--include=\*\." (project-file-extensions-string)))

(print (project-file-extensions-string))
(print (project-file-extensions-string-wildcards))
(print (project-file-extensions-string-include-wildcards))

;; Scans the drive starting at the specified root and returns
;; the list of files in the current project.
(defun get-project-file-cache (root)
  "Cache the files in the project for ido fast find"
  (interactive)
  (progn
    (cd root)
    (message (concat "Caching files for project " root "..."))
    (let ((cache (split-string 
                  (shell-command-to-string 
                   (concat
                    "cmd.exe /c dir /s /b " (project-file-extensions-string-wildcards))) "\n")))
      (delete "" cache)  ;; The above code generates a nil as the last entry, so remove it
      (message (concat "Done caching files for " root))
      cache)))

(defun set-project (root dfpp)
  (interactive "sProject root: \nsdfp-project: ")
  (let ((filecache (get-project-file-cache root)))
    (setq project root)
    (put 'project 'root root)
    (put 'project 'filecache filecache)
    (put 'project 'dfp-project dfpp)))

(defun codegrep (prompt)
 (interactive (list (read-string (format "Grep code in \%s for: " (get 'project 'root)))))
;; (if (get-buffer "*grep*") ; If old grep window exists
;;     (progn 
;;       ;;(delete-windows-on (get-buffer "*grep*")) ; delete the grep windows
;;       ;;(kill-buffer "*grep*") ; and kill the buffers
;;       )
;;   )
 (compilation-start 
  (format "grep -n -r \%s \%s \%s." (project-file-extensions-string-include-wildcards) prompt (get 'project 'root))
  'grep-mode))

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
      (setq key (replace-regexp-in-string (get 'project 'root) "" key))
      ;; remove trailing | or /
      (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
      (puthash key path tbl)
      (push key ido-list)
      )
    (get 'project 'filecache)
    )
  (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))
(global-set-key (kbd "C-S-f") 'my-ido-project-files)

(defun foreslash-to-backslash (str) (replace-regexp-in-string "/" "\\" str t t))
(defun backslash-to-foreslash (str) (replace-regexp-in-string "\\\\" "/" str t t))

;; Compile a Double Fine shader.
(defun compile-shader ()
  "Compile a Double Fine shader"
  (interactive)
  (progn
    ;; Get shader files
    (setq shader-files
          (split-string 
           (shell-command-to-string 
            (concat "cmd.exe /c dir /s /b " (foreslash-to-backslash (concat (get 'project 'dfp-project) "\\Code\\ShaderLibrary\\Src\\*.fx"))))
           "\n"))

    ;; Ask which shader to compile
    (setq shader-file (ido-completing-read "shader: " shader-files))

    ;; Compile
    (setq command (concat (get 'project 'dfp-project) "/win/bin/ShaderCompiler_win.exe " shader-file))
    (setq compilation-scroll-output 'true)
    (compilation-start command)))

;; Redo the last compile-shader command.
(defun recompile-shader ()
  "Compile a Double Fine shader"
  (interactive)
  (progn
    (setq command (concat (get 'project 'dfp-project) "/win/bin/ShaderCompiler_win.exe " shader-file))
    (setq compilation-scroll-output 'true)
    (compilation-start command)))

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
      (dolist (f (get 'project 'filecache))
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

(defun remove-trailing-foreslash (path)
  (let*
      ((index-of-last-char (- (length path) 1))
       (last-char (aref path index-of-last-char)))
    (if (eq last-char ?/)
        (substring path 0 index-of-last-char)
      path)))

;; Given a path, see if it matches any of the projects. If so, returns the project root.
(defun project-matches (path)
  (let ((path (remove-trailing-foreslash path)))
    (if (string= path (get 'project 'root))
        path
      nil)))

(project-matches "c:/dfp-mc/")

;; Given an a filename, return its project root by looping through the projects and finding the first one that matches.
(defun get-project-root (filename)
  (let*
      ((path (file-name-directory (backslash-to-foreslash filename)))
       (parts (split-string path "/"))
       (partial-path ())
       (match ()))
    (progn
      (while (and (car parts) (not match))
        (setq partial-path (concat partial-path (car parts) "/"))
        ;;(insert partial-path "\n")
        (setq match (project-matches partial-path))
        (setq parts (cdr parts)))
      match)))

(print (get-project-root buffer-file-name))


