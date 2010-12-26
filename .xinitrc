#!/bin/sh
# ~/.xinitrc
# Executed by startx (run your window manager from here)

xrdb ~/.Xresources

# obviate the need for .xmodmap jazz
setxkbmap -option ctrl:swapcaps

# C-M-BACKSPACE was removed from Xorg's default config. Put it back.
setxkbmap -option terminate:ctrl_alt_bksp

#xset fp+ ~/.fonts
#xset fp rehash

xsetroot -mod 2 2 -fg darkslategray -bg darkseagreen
xsetroot -cursor_name left_ptr

ck-launch-session /usr/bin/openbox-session

