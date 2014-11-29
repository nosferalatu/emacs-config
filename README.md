These files are my Emacs configuration. They are a mixture of Lisp I've written and third-party packages I've
downloaded. The latter are in the thirdparty/ directory. I need to look into using ELPA for package management.

I'm using Emacs 24.4 on Windows installed at c:\emacs.

These files are meant to go into the Emacs site-lisp directory. I have all my Emacs files in one place under the
c:\emacs directory, so I don't use a .emacs or .emacs.d. Instead, I put everything in site-lisp, and have a site-start.el
that runs the config code. This makes it is very simple to copy Emacs to a new computer-- just copy
everything in the c:\emacs directory.

The files in site-lisp are loaded before any .emacs and .emacs.d files.

To set this up, download Emacs, and clone the repo:
```
git clone https://github.com/nosferalatu/emacs-config.git c:/emacs/share/emacs/site-lisp
```
