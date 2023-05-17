local wezterm = require 'wezterm'
local act = wezterm.action

local key_mapping = {
  {
    key = 'x',
    mods = 'LEADER',
    action = act.ActivateCommandPalette,
  },
  {
    key = 'l',
    mods = 'LEADER',
    action = act.ShowLauncher
  },
  {
    key = '-',
    mods = 'LEADER',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = '=',
    mods = 'LEADER',
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'o',
    mods = 'LEADER',
    action = act.PaneSelect
  },
  {
    key = 'n',
    mods = 'LEADER',
    action = act.ActivateTabRelative(1)
  },
  {
    key = 'p',
    mods = 'LEADER',
    action = act.ActivateTabRelative(-1)
  },
  {
    key = 'c',
    mods = 'LEADER',
    action = act.SpawnTab 'CurrentPaneDomain'
  },
  {
    key = '[',
    mods = 'LEADER',
    action = act.ActivateCopyMode
  },
}

local copy_mode = {
  {
    key = 'p',
    mods = 'CTRL',
    action = act.CopyMode 'MoveUp'
  },
  {
    key = 'n',
    mods = 'CTRL',
    action = act.CopyMode 'MoveDown'
  },
  { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
  {
    key = 'Space',
    mods = 'NONE',
    action = act.CopyMode { SetSelectionMode = 'Cell' },
  },

  { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
  { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
  { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
  { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },

  { key = 'f', mods = 'CTRL', action = act.CopyMode 'MoveRight' },
  { key = 'b', mods = 'CTRL', action = act.CopyMode 'MoveLeft' },
  { key = 'v', mods = 'ALT', action = act.CopyMode 'PageUp' },
  { key = 'v', mods = 'CTRL', action = act.CopyMode 'PageDown' },
  {
    key = 'y',
    mods = 'NONE',
    action = act.Multiple {
      { CopyTo = 'ClipboardAndPrimarySelection' },
      { CopyMode = 'Close' },
    },
  },
  {
    key = 'w',
    mods = 'ALT',
    action = act.Multiple {
      { CopyTo = 'ClipboardAndPrimarySelection' },
      { CopyMode = 'Close' },
    },
  },
}

local search_mode = {
  { key = 'Enter', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
  { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
  { key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
  { key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
  { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
  { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
  {
    key = 'PageUp',
    mods = 'NONE',
    action = act.CopyMode 'PriorMatchPage',
  },
  {
    key = 'PageDown',
    mods = 'NONE',
    action = act.CopyMode 'NextMatchPage',
  },
  { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
  { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
}

local function init_module()
end

local module = {}

function module.apply_to_config(config)
  init_module()

  -- disable default key mappings
  config.disable_default_key_bindings = true
  config.leader = { key = 'a', mods = 'CTRL|ALT', timeout_milliseconds = 1000 }
  config.keys = key_mapping

  config.key_tables = {
    copy_mode = copy_mode,
    search_mode = search_mode
  }
end

return module
