;; Create c:/test.txt with contents "hello world".
(progn
  (save-excursion
    (find-file "c:/test.txt")
    (erase-buffer)
    (insert "hello world")
    (save-buffer)
    (kill-buffer)))

;; Open c:/test.txt and replace "hello" with "howdy".
(progn
  (save-excursion
    (find-file "c:/test.txt")
    (setq from-string "hello")
    (setq to-string "howdy")
    (beginning-of-buffer)
    (while (search-forward from-string nil t)
      (replace-match to-string nil t))
    (save-buffer)
    (kill-buffer)))

