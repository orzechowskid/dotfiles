#!/bin/bash

# register this file with gnome-session-properties to run upon login
# or add a .desktop entry to ~/.config/autostart

xrdb -merge $HOME/.Xresources

gsettings set org.gnome.desktop.background picture-options none
gsettings set org.gnome.desktop.background primary-color "#082f3e"

