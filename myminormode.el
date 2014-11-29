(defvar my-minor-mode nil)

(defvar my-minor-mode-project-name nil)

(defun my-minor-mode-init ()
  (t))

(defun my-minor-mode-modeline ()
  '" PRJ: c:/dfp-seed")

(define-minor-mode my-minor-mode "my minor mode"
  :lighter nil
;;  (setq my-minor-mode-project-name "PRJ:c:/dfp-seed")
  (if my-minor-mode
      (insert "enabling")
    (insert "disabling")))

disablingenabling
disablingenabling
enablingdisabling
enablingdisabling
enablingdisabling
enablingdisabling
enablingdisabling



(print mode-line)
(print (mode-line))
(print mode-line-format)

(setq mode-line-format (cons "PRJ:c:/dfp-seed" mode-line-format))
(setq mode-line-format (cons (my-minor-mode) mode-line-format))
(setq mode-line-format (cdr mode-line-format))



(setq foobartest "blah")

(put 'foobartest 'prop "some prop a")

(get 'foobartest 'prop)

project-directory
(get 'project 'root)

dfp-project
(get 'project 'filecache)

project-file-cache
(get 'project 'dfp-project)
