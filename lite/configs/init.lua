local utils = require "configs.utils"

local plugins = require "configs.plugins"
local theme = require "configs.theme"
local keymap = require "configs.keymap"

if utils.fileExists(USERDIR .. '/configs/' .. PLATFORM) then
  local plat = require ('configs.' .. PLATFORM)
end
