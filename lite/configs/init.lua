local config = require "core.config"

-- Enable wrapping automatically when opening files
config.plugins.linewrapping.enable_by_default = true

-- Wrapping mode: "letter" (default) or "word"
config.plugins.linewrapping.mode = "word"

-- Follow indentation level of the wrapped line (default: true)
config.plugins.linewrapping.indent = true

-- Show vertical guide line where wrapping occurs (default: true)
config.plugins.linewrapping.guide = true

local utils = require "configs.utils"

local plugins = require "configs.plugins"
local theme = require "configs.theme"
local keymap = require "configs.keymap"
local commands = require "configs.commands"

if utils.fileExists(USERDIR .. '/configs/' .. PLATFORM) then
  local plat = require ('configs.' .. PLATFORM)
end
