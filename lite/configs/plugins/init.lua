local core = require "core"
local config = require "core.config"

config.plugins.miq.debug = false
config.plugins.miq.repos = {
    'https://github.com/lite-xl/lite-xl-plugins.git:master',
    'https://github.com/Evergreen-lxl/evergreen-languages.git:main',
}

config.plugins.miq.plugins = {
  -- this allows Miq to manage itself
  'TorchedSammy/Miq',

  'editorconfig',
  'cleanstart',
  'indentguide',

  'Evergreen-lxl/Evergreen.lxl',
  'evergreen_c',
  'evergreen_cpp',
  'evergreen_lua',
  'evergreen_python',
  'evergreen_json',
  'evergreen_bash',
  'evergreen_cmake',
  'evergreen_rust',
}

-- rg-search: ripgrep executable and flags.
-- extra_flags are inserted between the executable and the fixed "--vimgrep -- <query> <root>".
-- config.plugins.rgsearch = {
--   executable  = "rg",
--   extra_flags = { "--smart-case", "--follow" },
-- }

-- fd-files: fd executable, flags, and result cap.
-- extra_flags are inserted between the executable and "--max-results <n> . <root>".
-- config.plugins.fd_files = {
--   executable  = "fd",
--   extra_flags = { "--type", "f", "--follow" },
--   max_results = 500,
-- }

-- killring: maximum number of clipboard entries kept in the ring.
-- config.plugins.killring = {
--   max_entries = 100,
-- }

-- listview: number of result rows visible in the overlay panel.
-- config.plugins.listview = {
--   rows = 10,
-- }
