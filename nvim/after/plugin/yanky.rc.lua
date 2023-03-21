local status, yanky = pcall(require, "yanky")

if (not status) then
  print('yanky not loaded')
  return
end

local utils = require("yanky.utils")
local mapping = require("yanky.telescope.mapping")
local actions = require('telescope.actions')

yanky.setup({
  highlight = {
    on_put = false,
    on_yank = false,
    timer = 100,
  },
  picker = {
    telescope = {
      mappings = {
        default = mapping.put("p"),
        i = {
          ["<c-n>"] = actions.move_selection_next,
          ["<c-p>"] = actions.move_selection_previous,
          ["<c-j>"] = mapping.put("p"),
          ["<c-k>"] = mapping.put("P"),
          ["<c-x>"] = mapping.delete(),
          ["<c-r>"] = mapping.set_register(utils.get_default_register()),
        },
        n = {
          p = mapping.put("p"),
          P = mapping.put("P"),
          d = mapping.delete(),
          r = mapping.set_register(utils.get_default_register())
        },
      }
    }
  }
})

