-- put user settings here
-- this module will be loaded after everything else when the application starts
-- it will be automatically reloaded when saved
local core = require "core"
local config = require "core.config"
config.skip_plugins_version = true

local configs = require "configs"
