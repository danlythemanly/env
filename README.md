env
===

This is my crazy emacs/terminal environment.

Emacs key bindings are ingrained in my brain and fingers, and I
started to find it irritating to switch back and forth and copy-paste
between (many) terminals and emacs buffers.  Now instead of starting a
terminal I actually run 

    emacsclient -t -e "(bash \"*term*\")" -a ""

Note: I still like to run emacs in terminal mode to make it easier for
me to copy-paste from other applications (e.g., web browsers).

Notable things in my .emacs:

* start a new terminal (named whatever you like) with 

    M-N

* switch between line and char mode in the terminal with

    M-M

* move between buffer windows (terminals or not) with

    M-J, M-I, M-K, M-L

* close terminal buffer on Ctrl-D (process exit)

* increase and decrease terminal size for xfce terminals (uses script
  in ~/bin)

    M-+, M-_

Since I find myself typing 'emacs' a lot and don't want to start emacs
within emacs, I also have a few bash aliases to convert things to
emacs lisp expressions.  For example, 

    alias emacs='emacsclient-find'

where emacsclient-find is just 

    emacsclient -e "(find-file \"$1\")"

To use the bash aliases, I just add to my .bashrc the following:

    . ~/projects/env/bash_android
    . ~/projects/env/bash_emacs

