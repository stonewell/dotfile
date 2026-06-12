local up     = require 'plugins.use_package'   -- must be first
local config = require 'core.config'

up.repos {
  'https://github.com/lite-xl/lite-xl-plugins.git:master',          -- editorconfig, cleanstart, indentguide
}

-- repo installs (plain name → searched in registered manifests above)
up.use 'editorconfig'
up.use 'cleanstart'
up.use 'indentguide'

-- treesit: local plugin from dotfile — config runs after all plugins have loaded
up.use {
  plugin = USERDIR .. '/plugins/treesit',
  name   = 'treesit',
  config = function()
    -- Neovim paths for this machine (scoop install + lazy.nvim data dir)
    config.plugins.treesit.nvimTsRoot           = 'C:/depot/stone/nvim-data/lazy/nvim-treesitter'
    config.plugins.treesit.nvimRuntimeDir       = 'C:/depot/scoop/apps/neovim/current/share/nvim/runtime'
    config.plugins.treesit.nvimBuiltinParserDir = 'C:/depot/scoop/apps/neovim/current/lib/nvim/parser'

    local languages = require 'plugins.treesit.languages'
    local ts = config.plugins.treesit

    -- Helper: nvim-treesitter query + a custom parser dir
    local function nvimWith(parserDir, name, files)
      languages.addNvimLang {
        root       = ts.nvimTsRoot,
        runtimeDir = ts.nvimRuntimeDir,
        parserDir  = parserDir,
        name       = name,
        files      = files,
      }
    end

    -- Bundled Neovim parsers — available immediately, no :TSInstall needed
    -- (c, lua, markdown, markdown_inline, vim, vimdoc shipped with Neovim 0.12)
    local builtin = ts.nvimBuiltinParserDir
    nvimWith(builtin, 'c',                { '%.c$', '%.h$' })
    nvimWith(builtin, 'lua',              { '%.lua$' })
    nvimWith(builtin, 'markdown',         { '%.md$', '%.markdown$' })
    nvimWith(builtin, 'markdown_inline',  {})  -- injected by markdown, no direct files
    nvimWith(builtin, 'vim',              { '%.vim$' })
    nvimWith(builtin, 'vimdoc',           { '%.txt$' })

    -- TSInstall-ed parsers — run :TSInstall <lang> in Neovim first,
    -- then uncomment the corresponding line below.
    local function nvim(name, files)
      languages.addNvimLang {
        root = ts.nvimTsRoot,
        runtimeDir = ts.nvimRuntimeDir,
        name = name,
        files = files,
      }
    end
    -- nvim('python',     { '%.py$' })
    -- nvim('javascript', { '%.js$', '%.jsx$' })
    -- nvim('typescript', { '%.ts$', '%.tsx$' })
    -- nvim('cpp',        { '%.cpp$', '%.cxx$', '%.cc$', '%.hpp$', '%.hh$' })
    -- nvim('rust',       { '%.rs$' })
    -- nvim('go',         { '%.go$' })
  end,
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
