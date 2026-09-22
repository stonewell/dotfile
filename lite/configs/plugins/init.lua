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

-- treesit: local plugin from lite-xl-plugins repo — config runs after all plugins have loaded
up.use {
  plugin = 'treesit',
  name   = 'treesit',
  config = function()
    -- Neovim paths (nvimTsRoot, nvimRuntimeDir, nvimBuiltinParserDir) are
    -- auto-detected across Windows, Linux, and macOS (Scoop, Winget, Homebrew,
    -- distro packages, etc.). Override config.plugins.treesit.* here only if
    -- using a non-standard path.
    local languages = require 'plugins.treesit.languages'
    local ts = config.plugins.treesit

    -- Register language grammars.
    -- Searches both Neovim bundled parsers (c, lua, markdown, vim, vimdoc)
    -- and :TSInstall-ed parsers (python, cpp, rust, go, objc, etc.).
    -- If a tree-sitter parser is not installed, it automatically falls back
    -- to compatible parent parsers (e.g. objcpp -> objc -> c, cpp -> c) or
    -- to Lite-XL's built-in syntax engine without errors, matching Neovim.
    local function nvim(name, files, extra)
      local spec = {
        root = ts.nvimTsRoot,
        runtimeDir = ts.nvimRuntimeDir,
        name = name,
        files = files,
      }
      if extra then
        for k, v in pairs(extra) do spec[k] = v end
      end
      languages.addNvimLang(spec)
    end

    -- Bundled Neovim parsers (available immediately)
    nvim('c',                { '%.c$', '%.h$' })
    nvim('lua',              { '%.lua$' })
    nvim('markdown',         { '%.md$', '%.markdown$' })
    nvim('markdown_inline',  {})  -- injected by markdown, no direct files
    nvim('vim',              { '%.vim$', '%.vimrc$' })
    nvim('vimdoc',           { '[/\\]doc[/\\].*%.txt$', '^doc[/\\].*%.txt$' })
    nvim('query',            { '%.scm$' })

    -- Additional languages (tree-sitter when installed, otherwise built-in syntax)
    nvim('cmake',            { '%.cmake$', '%.cmake%.in$', '^[Cc][Mm]ake[Ll]ists%.txt$', '[/\\][Cc][Mm]ake[Ll]ists%.txt$' })
    nvim('python',           { '%.py$' })
    nvim('javascript',       { '%.js$', '%.jsx$' })
    nvim('typescript',       { '%.ts$', '%.tsx$' })
    nvim('cpp',              { '%.cpp$', '%.cxx$', '%.cc$', '%.hpp$', '%.hh$' })
    nvim('rust',             { '%.rs$' })
    nvim('go',               { '%.go$' })
    nvim('objc',             { '%.m$' })
    nvim('objcpp',           { '%.mm$', '%.M$' })
    nvim('bash',             { '%.sh$', '%.bash$', '%.zsh$', '%.fish$', '^%.bashrc$', '[/\\]%.bashrc$', '^%.zshrc$', '[/\\]%.zshrc$', '^%.profile$', '[/\\]%.profile$' })
    nvim('make',             { '^[Mm]akefile$', '^GNUmakefile$', '[/\\][Mm]akefile$', '[/\\]GNUmakefile$', '%.mk$', '%.mak$' })
    nvim('json',             { '%.json$', '%.cjson$', '%.jsonc$', '%.json5$', '%.ipynb$' })
    nvim('java',             { '%.java$' })
    nvim('kotlin',           { '%.kt$', '%.kts$' })
    nvim('dockerfile',       { '^[Dd]ockerfile.*', '[/\\][Dd]ockerfile.*', '%.dockerfile$' })
    nvim('c_sharp',          { '%.cs$' })
    nvim('powershell',       { '%.ps1$', '%.psm1$', '%.psd1$' })
    nvim('diff',             { '%.diff$', '%.patch$', '%.rej$' })
    nvim('ini',              { '%.ini$', '%.inf$', '%.cfg$', '%.conf$', '^%.editorconfig$', '[/\\]%.editorconfig$' })
    nvim('php',              { '%.php$', '%.phtml$' })
    nvim('ruby',             { '%.rb$', '^Rakefile$', '[/\\]Rakefile$', '^Gemfile$', '[/\\]Gemfile$', '%.gemspec$' })
    nvim('sql',              { '%.sql$', '%.psql$', '%.pgsql$' })
    nvim('zig',              { '%.zig$', '%.zon$' })
    nvim('swift',            { '%.swift$' })
    nvim('html',             { '%.html$', '%.htm$' })
    nvim('css',              { '%.css$' })
    nvim('yaml',             { '%.yaml$', '%.yml$' })
    nvim('toml',             { '%.toml$' })

    -- Re-apply treesit to any documents that were already open at startup
    -- (Doc:new runs before this config callback, so those docs missed the lang defs)
    local highlights = require 'plugins.treesit.highlights'
    for _, doc in ipairs(core.docs) do
      if not doc.treesit and doc.filename then
        highlights.init(doc)
        if doc.treesit then
          doc.highlighter:reset()
        end
      end
    end
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
