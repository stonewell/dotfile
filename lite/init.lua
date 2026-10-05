-- put user settings here
-- this module will be loaded after everything else when the application starts
-- it will be automatically reloaded when saved
local core = require "core"
local config = require "core.config"
config.skip_plugins_version = true

-- Backward compatibility for plugins expecting core.project_dir / core.project_directories (Lite-XL 2.0 API)
if not core.project_dir then
  setmetatable(core, {
    __index = function(t, k)
      if k == "project_dir" then
        local p = core.root_project and core.root_project()
        return p and p.path or "."
      elseif k == "project_directories" then
        local dirs = {}
        for _, pr in ipairs(core.projects or {}) do
          table.insert(dirs, { name = pr.path, path = pr.path })
        end
        return dirs
      end
      return rawget(t, k)
    end
  })
end

local configs = require "configs"

-- Local (machine specific) configuration: USERDIR/local.lua, loaded if it exists
local utils = require "configs.utils"

if utils.fileExists(USERDIR .. PATHSEP .. "local.lua") then
  local ok, err = pcall(require, "local")
  if not ok then
    core.error("Failed to load local.lua: %s", err)
  end
end

