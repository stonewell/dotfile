local status, telescope = pcall(require, "telescope")
if (not status) then return end
local actions = require('telescope.actions')
local builtin = require("telescope.builtin")

local function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local fb_actions = require "telescope".extensions.file_browser.actions

telescope.setup {
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close,
     },
    },

    file_ignore_patterns = {
      "node_modules",
      "[.]git/"
    },
  },
  extensions = {
    file_browser = {
      theme = "dropdown",
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,

      -- default options for browser file
      previewer = false,
      respect_gitignore = false,
      hidden = true,
      grouped = true,
      initial_mode = "normal",
      layout_config = { height = 40 },

      mappings = {
        -- your custom insert mode mappings
        ["i"] = {
          ["<C-w>"] = function() vim.cmd('normal vbd') end,
        },
        ["n"] = {
          -- your custom normal mode mappings
          ["N"] = fb_actions.create,
          ["h"] = fb_actions.goto_parent_dir,
          ["/"] = function()
            vim.cmd('startinsert')
          end
        },
      },
    },

    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },

    yank_history = {

    },
  },
}

telescope.load_extension("file_browser")
telescope.load_extension("fzf")
telescope.load_extension("yank_history")
telescope.load_extension("projects")

vim.keymap.set('n', ';f',
  function()
    builtin.find_files({
      cwd = telescope_buffer_dir(),
      no_ignore = false,
      hidden = true
    })
  end, {desc="Find Fies in Current Buffer Directory"})
vim.keymap.set('n', ';r', function()
  builtin.live_grep({
    cwd = telescope_buffer_dir(),
  })
end, {desc="Live Grep Current Buffer Directory"})
vim.keymap.set('n', ';p', function()
  builtin.grep_string({
    cwd = telescope_buffer_dir(),
  })
end, {desc="Grep String in Current Buffer Directory"})
vim.keymap.set('n', ';b', function()
  builtin.buffers()
end, {desc="Buffer List"})

if not is_win then
  vim.keymap.set('n', ';t', function()
    builtin.treesitter()
  end, {desc="Treesitter Symbols"})
end

vim.keymap.set('n', ';;', function()
  builtin.resume()
end, {desc="Resume Last Picker"})
vim.keymap.set('n', ';e', function()
  builtin.diagnostics()
end, {desc="Diagnostics"})
vim.keymap.set("n", "sf", function()
  telescope.extensions.file_browser.file_browser({
    path = "%:p:h",
    cwd = telescope_buffer_dir()
 })
end, {desc="Browse Files"})
vim.keymap.set("n", ";y", function()
  telescope.extensions.yank_history.yank_history({
  })
end, {desc="Yank History"})
vim.keymap.set("n", "sp", function()
  telescope.extensions.projects.projects({
  })
end, {desc="Projects"})
vim.keymap.set("n", "sr", "<cmd>Telescope oldfiles<cr>", {desc="Projects"})


