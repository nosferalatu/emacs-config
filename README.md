These files are my Emacs configuration. They are a mixture of Lisp I've written and third-party packages I've
downloaded. The latter are in the thirdparty/ directory.

I'm using Emacs 24.4 on Windows. I often switch between machines, so I like to keep Emacs portable and self-contained. I
don't use a .emacs file or the .emacs.d directory; instead, I have all of my Emacs Lisp files in [Emacs installation
dir]/elisp, and set up the file site-lisp/site-start.el to launch config.el in those files. This makes it very simple to
copy Emacs to a new computer, because you can just copy everything in c:/emacs to a new directory and launch
c:/emacs/bin/emacs.exe.

I have everything in my elisp directory under version control on Github.

* To set this up, download Emacs, and clone the repo into a directory named elisp:
```
git clone https://github.com/nosferalatu/emacs-config.git c:/emacs/elisp
```

* Then set up your site-start.el. For Emacs 24.4, this is in c:/emacs/share/emacs/site-lisp/site-start.el:
```
(let*
    ((dirs (split-string exec-directory "/"))
     (elisp-dir (concat (car dirs) "/" (nth '1 dirs) "/elisp/")))  ;; set elisp-dir to e.g. c:/emacs/elisp/
  (load (concat elisp-dir "config.el")))
```
