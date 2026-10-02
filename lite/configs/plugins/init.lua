local up     = require 'plugins.use_package'   -- must be first
local config = require 'core.config'
local core = require "core"

up.auto_install = true
up.auto_update = true

up.repos {
  'https://github.com/lite-xl/lite-xl-plugins.git:master',          -- editorconfig, cleanstart
  'https://github.com/stonewell/lite-xl-plugins.git:main',
}

-- repo installs (plain name → searched in registered manifests above)
up.use 'editorconfig'
up.use 'cleanstart'
up.use 'avy'
up.use 'bufferex'
up.use 'emacs'
up.use 'fd-files'
up.use 'indentguideex'
up.use 'isearch'
up.use 'killring'
up.use 'rgsearch'
up.use 'whichkey'

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

-- scale: default mode ("code") only live-rescales style.code_font when the
-- display scale changes (e.g. moving the window to a different-DPI
-- monitor) -- style.font (used by which-key, and the rgsearch/fd-files/
-- killring/bufferex listview popups) and SCALE itself are left untouched,
-- so those end up too small relative to the now-correctly-scaled code
-- text. "ui" mode rescales everything (fonts, padding, scrollbars, SCALE)
-- together instead. Field assignment (not a full table replace) so this
-- works regardless of whether this file or the scale plugin's own
-- defaults-merge runs first.
config.plugins.scale = config.plugins.scale or {}
config.plugins.scale.mode = "ui"
