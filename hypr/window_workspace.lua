-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Example window rules that are useful

local suppressMaximizeRule = hl.window_rule({
    -- Ignore maximize requests from all apps. You'll probably like this.
    name  = "suppress-maximize-events",
    match = { class = ".*" },

    suppress_event = "maximize",
})
-- suppressMaximizeRule:set_enabled(false)

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

hl.window_rule({
  match = {
    class = "(Firefox)"
  },
  workspace = "2 silent"
})
hl.window_rule({
  match = {
    class = "(firefox)"
  },
  workspace = "2 silent"
})

hl.window_rule({
  match = {
    class = "(weixin)"
  },
  workspace = "10"
})
hl.window_rule({
  match = {
    class = "(wechat)"
  },
  workspace = "10"
})

hl.window_rule({
  match = {
    class = "(sdl-freerdp)"
  },
  fullscreen_state = "3"
})

hl.workspace_rule({
  workspace = "10",
  monitor = "HDMI-A-1",
  default = true,
  persistent = true,
  layout_opts = {
    orientation = "top"
  }
})
