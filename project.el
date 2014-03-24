;; The following functions are useful:
;; add-project
;; remove-project
;; grep-project
;; find-file-in-project
;; find-matching-file
;; compile-shader
;; recompile-shader

;; There is one global variable called projects. projects is a hash table whose keys are the project roots (such as "c:/dfp-seed") and the value is another hash table with the following entries:
;; root: the root of the project (same as the key in the projects hash table)
;; filecache: cache of the filenames in the project
;; dfp-project: double fine root (e.g. root of "c:/dfp-mc" has double fine root of "c:/dfp-mc/mc")

;; TODO:
;; specify filters in projectgrep? make a list of prompts and filters (such as (("All" . ".cpp .c .h") ("Lua" . ".lua") ("Shader" . ".fx .fxh"))) and ask the user which to use in the grep-code function
;; automatically find shadercompiler and eliminate dfp-project

(setq projects (make-hash-table :test 'equal))

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

;; Scans the drive starting at the specified root and returns
;; the list of files in the current project
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

;; Adds a new project to the global table of projects
;; Caches the files used in the project using get-project-file-cache
;; If the project is already in the list, the old one is replaced with the new one (so it recaches the files)
(defun add-project (root dfpp)
  (interactive "sProject root: \nsdfp-project: ")
  (let ((filecache (get-project-file-cache root)))
    (setq project (make-hash-table))
    (puthash 'root root project)
    (puthash 'filecache filecache project)
    (puthash 'dfp-project dfpp project)
    (puthash root project projects)))

(defun delete-project ()
  (interactive)
  (let
      ((project-to-remove (query-for-project "Remove")))
    (remhash (gethash 'root project-to-remove) projects)))

;; Return a list of all the keys in the given hashtable
(defun keys (hashtable)
  (let (allkeys)
    (maphash (lambda (key value) (setq allkeys (cons key allkeys))) hashtable)
    allkeys))

;; Return a list of all the project roots. It puts the current buffer's project at the front of the list so that it's the default for ido-completing-read.
(defun get-project-root-list ()
  ;; Remove nil entries (if the current buffer's not in a project, then get-project-root-for-buffer returns nil, which we don't want in the list of project roots)
  (delq nil 
        ;; Delete any duplicates in the list
        (delete-dups 
         ;; Create a list where the first entry is this buffer's project followed by the rest of the projects
         (cons (get-project-root-for-buffer) (keys projects)))))

;; Query the user for which project to use. If there is only one project in the projects variable, then return that without prompting.
(defun query-for-project (prompt)
  (let*
      ((project-root-list (get-project-root-list))
       (project-root))
    (if (eq 1 (length project-root-list))
        (setq project-root (car project-root-list))
      (setq project-root (ido-completing-read (concat prompt " project: ") project-root-list)))
    (gethash project-root projects)))

(defun grep-project ()
  (interactive)
  (let*
      ((selected-project (query-for-project "Grep in"))
       (grep-string (read-string (format "Grep in \%s for: " (gethash 'root selected-project)))))
;; (if (get-buffer "*grep*") ; If old grep window exists
;;     (progn 
;;       ;;(delete-windows-on (get-buffer "*grep*")) ; delete the grep windows
;;       ;;(kill-buffer "*grep*") ; and kill the buffers
;;       )
;;   )
    (compilation-start 
     (format "grep -n -r \%s \%s \%s" (project-file-extensions-string-include-wildcards) grep-string (gethash 'root selected-project))
     'grep-mode)))
(global-set-key (kbd "C-S-s") 'grep-project)

;; Always open *grep* windows in the current window; inhibit creating a new one
;; (add-to-list 'same-window-buffer-names "*grep*")

;; Use Ido to find files. The list of files is the project-file-cache variable.
(defun find-file-in-project ()
  "Use ido to select a file from the project."
  (interactive)
  (let 
      ((selected-project (query-for-project "Find file in"))
       (tbl (make-hash-table :test 'equal))
       (ido-list))
    (mapc (lambda (path)
            ;; format path for display in ido list
            (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
            ;; strip project root
            (setq key (replace-regexp-in-string (gethash 'root selected-project) "" key))
            ;; remove trailing | or /
            (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
            (puthash key path tbl)
            (push key ido-list)
            )
          (gethash 'filecache selected-project))
    (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl))))
(global-set-key (kbd "C-S-f") 'find-file-in-project)

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

(defun remove-trailing-foreslash (path)
  (let*
      ((index-of-last-char (- (length path) 1))
       (last-char (aref path index-of-last-char)))
    (if (eq last-char ?/)
        (substring path 0 index-of-last-char)
      path)))

;; Given a path, see if it matches any of the projects. If so, returns the project hash table entry.
(defun project-matches (path)
  (let 
      ((path (remove-trailing-foreslash path))
       (match))
    (maphash
     (lambda (key value)
       (if (string= key path)
           (setq match value)))
     projects)
    match))

;; Given a filename, return the project that contains this filename. It does this by breaking up the filename into its subdirectories,
;; going backwards through those subdirectories, and testing them against each project root.
(defun get-project (filename)
  (let*
      ((path (file-name-directory (backslash-to-foreslash filename)))
       (parts (split-string path "/"))
       (partial-path ())
       (match ()))
    (progn
      (while (and (car parts) (not match))
        (setq partial-path (concat partial-path (car parts) "/"))
        (setq match (project-matches partial-path))
        (setq parts (cdr parts)))
      match)))

;; Returns this buffer's project's hash table
(defun get-project-for-buffer ()
  (get-project buffer-file-name))

;; Returns this buffer's project's root, or nil if the buffer isn't inside a project
(defun get-project-root-for-buffer ()
  (let ((proj (get-project-for-buffer)))
    (if (hash-table-p proj)
        (gethash 'root proj)
      nil)))



;; The following functions implement "find matching file" functionality using the project information.
;; A matching file means e.g. the .h that matches the .cpp and vice versa.
;; When you are on a #include line, it instead opens the file that the #include references.
;; It uses other-file-alist to associate matching file extensions. Also, it uses the buffer's current project
;; to quickly find the matching file.

;; Given a filename, return the matching filename using the other-file-alist.
;; Returns nil if the filename can't be matched.
(defun get-file-matching-extension (file)
  (let (extension matching-extension matching-file)
    (setq extension (file-name-extension file))
    (when (setq matching-extension (cdr (assoc (concat "." extension) other-file-alist)))
      (setq matching-file (concat (file-name-sans-extension file) matching-extension)))))

;; Returns the matching filename of the current buffer, or if the cursor is on a #include line, the #include path.
;; Returns nil if the filename can't be matched.
(defun get-include-path-or-matching-filename ()
  (let (line file extension matching-extension)
    (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (if (string-match "^\#\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]" line)
        (setq file (match-string 2 line))
      (setq file (get-file-matching-extension(file-name-nondirectory buffer-file-name))))
    file))

;; Given a filename (possibly including a partial path), return a list of all the project files with that name and partial path.
;; "partial path" means DFGraphics/Inc/ in the filename DFGraphics/Inc/GraphicsManager.h.
(defun find-in-project (file project)
  (when (not (null file))
    (let ((result))
      (if (not (equal (substring file 0 1) "/"))
          (setq file (concat "/" file)))
      (dolist (f (gethash 'filecache project))
        (if (string-match file (backslash-to-foreslash f))
            (add-to-list 'result f)))
      result)))

;; Find the current buffer's matching file using the other-file-alist.
;; If the current list is a #include, find the file referenced by the #include.
(defun find-matching-file()
  (interactive)
  (let 
      ((project (get-project-for-buffer))
       (matching-filename) 
       (matches))
      (setq matching-filename (get-include-path-or-matching-filename))
      (if (not (null matching-filename))
          (progn
            (setq matches (find-in-project matching-filename project))
            (cond
             ((or (null matches) (= 0 (length matches)))
              (message (concat "Cannot find " matching-filename " in project " (gethash 'root project))))
             ((= 1 (length matches))
              (find-file (car matches)))
             (t 
              (find-file (ido-completing-read "Matching files: " matches)))))
          (message (concat "Unknown extension '." (file-name-extension buffer-file-name) "'")))))

;; How to match files
(setq other-file-alist
      '((".c"    . ".h")
        (".cpp"  . ".h")
        (".h"    . ".cpp")
        (".fx"   . ".fxh")
        (".fxh"  . ".fx")
))

(global-set-key [f3] 'find-matching-file) 
