#!/bin/bash
# 唤醒后恢复：等 NVIDIA 惰性显存恢复完成，再逐输出重新点亮。
# 由 hypridle 的 after_sleep_cmd 后台调用，允许较长等待。

sleep 8

# 双屏逐个 dpms off/on：resume 时 transform=1 的 HDMI-A-1 经常被摘掉。
for out in DP-1 HDMI-A-1; do
    hyprctl dispatch dpms off "$out" >/dev/null 2>&1
done
sleep 1
for out in DP-1 HDMI-A-1; do
    hyprctl dispatch dpms on "$out" >/dev/null 2>&1
done
sleep 1
hyprctl dispatch dpms on >/dev/null 2>&1

# 幂等拉起常驻进程。
pidof sway-audio-idle-inhibit >/dev/null || (sway-audio-idle-inhibit >/dev/null 2>&1 &)
pidof hyprpaper >/dev/null || (hyprpaper >/dev/null 2>&1 &)

# 壁纸轮换交给 hyprpaper 自身的 timeout=2520；如需手动换图用 pypr wallpapers next。

exit 0
