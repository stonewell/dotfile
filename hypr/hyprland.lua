------------------
---- MONITORS ----
------------------

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
hl.monitor(
  {
    output   = "DP-1",
    mode     = "preferred",
    position = "auto",
    scale    = "auto",
    cm = "auto",
  }
)
hl.monitor(
  {
    output   = "HDMI-A-1",
    mode     = "preferred",
    position = "auto-left",
    scale    = "auto",
    cm = "auto",
    transform = 1,
  }
)

-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/
hl.on("hyprland.start",
      function ()
        hl.exec_cmd("hypridle")
        hl.exec_cmd("hyprpaper")
        hl.exec_cmd("mako")
        hl.exec_cmd("sway-audio-idle-inhibit")
        hl.exec_cmd("fcitx5 -r")
        hl.exec_cmd("matcha -d -o")
        hl.exec_cmd("copyq --start-server")
        hl.exec_cmd("sleep 6;virt-manager --show-systray --fork")
      end
)


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("LIBVA_DRIVER_NAME","nvidia")
hl.env("XDG_SESSION_TYPE","wayland")
hl.env("GBM_BACKEND","nvidia-drm")
hl.env("__GLX_VENDOR_LIBRARY_NAME","nvidia")
hl.env("WLR_NO_HARDWARE_CURSORS","1")
hl.env("GDK_SCALE"," 2")

---------------------
---- Look and Feels ----
---------------------
require("general")

hl.config({
    general = {
        layout = "master",
    }
})

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config({
  master = {
    new_status = "master",
    mfact = 0.70,
  },
})

hl.config({
  xwayland = {
    force_zero_scaling = true
  }
})

---------------------
---- KEYBINDINGS ----
---------------------
require("keybinding")

--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

require("window_workspace")
