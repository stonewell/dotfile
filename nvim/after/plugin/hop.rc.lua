local status, telescope = pcall(require, "hop")
if (not status) then return end

local hop = require('hop')
local hop_hint = require('hop.hint')

vim.keymap.set('n', 'f',
  function()
    hop.hint_char2({
    })
  end)

vim.keymap.set('i', '<C-s>',
  function()
    hop.hint_patterns({
    })
  end)

vim.keymap.set('n', ';n',
  function()
    hop.hint_patterns({
    }, vim.fn.getreg('/'))
  end)
