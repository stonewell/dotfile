-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux
local launch_menu = require 'launcher'
local key_bindings = require 'key-bindings'

-- This table will hold the configuration.
local config = {
}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

wezterm.on('gui-startup', function()
 local tab, pane, window = mux.spawn_window({})
 window:gui_window():maximize()
end)

config.color_scheme = 'Darcula (base16)'
config.font = wezterm.font 'SauceCodePro Nerd Font Mono'
config.font_size = 14
config.window_decorations = "RESIZE"
config.use_dead_keys = false
config.scrollback_lines = 9999
config.adjust_window_size_when_changing_font_size = false
config.hide_tab_bar_if_only_one_tab = true

launch_menu.apply_to_config(config)
key_bindings.apply_to_config(config)

-- and finally, return the configuration to wezterm
return config
