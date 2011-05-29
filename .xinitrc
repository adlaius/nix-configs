#!/bin/sh
# ~/.xinitrc
# Executed by startx (run your window manager from here)

xrdb ~/.Xresources

# C-M-BACKSPACE was removed from Xorg's default config. Put it back.
setxkbmap -option terminate:ctrl_alt_bksp

# load xmodmap customizations
xmodmap ~/.xmodmap

#xsetroot -mod 11 6 -fg gray20 -bg black
#xsetroot -cursor_name left_ptr

ck-launch-session gnome-session

