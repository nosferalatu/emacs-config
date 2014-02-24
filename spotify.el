;; Simple way to control Spotify with Emacs
;; Requires Python; Emacs doesn't expose enough of the win32 API to do the FindWindow/SendMessage trick below
;; Also requires Python for Windows extensions
;; More info on Spotify here: http://stackoverflow.com/questions/8459162/user32-api-custom-postmessage-for-automatisation

(defun spotify-pause-play-toggle()
  (interactive)
  (save-excursion
    (find-file "c:/emacs/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 917504)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/emacs/spotifycommand.py")))

(defun spotify-next()
  (interactive)
  (save-excursion
    (find-file "c:/emacs/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 720896)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/emacs/spotifycommand.py")))

(defun spotify-prev()
  (interactive)
  (save-excursion
    (find-file "c:/emacs/spotifycommand.py")
    (erase-buffer)
    (insert 
"import win32api\n"
"import win32gui\n"
"spotify_hwnd = win32gui.FindWindow(\"SpotifyMainWindow\", None)\n"
"win32gui.SendMessage(spotify_hwnd, 0x0319, 0, 786432)\n"
    )
    (save-buffer)
    (kill-buffer)
    (shell-command "c:/emacs/spotifycommand.py")))

(global-set-key (kbd "C-M-<pause>") 'spotify-pause-play-toggle)
