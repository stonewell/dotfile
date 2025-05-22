#!/bin/bash

pidof sway-audio-idle-inhibit || sway-audio-idle-inhibit >/dev/null 2>&1 &
pidof hyprpaper || hyprpaper > /dev/null 2>&1 &
pypr wall next
