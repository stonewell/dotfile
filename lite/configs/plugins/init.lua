local up = require 'plugins.use_package'   -- must be first

up.repos {
  'https://github.com/lite-xl/lite-xl-plugins.git:master',          -- editorconfig, cleanstart, indentguide
}

-- git install (Author/Repo slug → cloned from GitHub)
up.use 'Evergreen-lxl/Evergreen.lxl'

-- repo installs (plain name → searched in registered manifests above)
up.use 'editorconfig'
up.use 'cleanstart'
up.use 'indentguide'

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
