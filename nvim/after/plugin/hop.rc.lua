local status, hop = pcall(require, "hop")
if (not status) then return end

local hop_hint = require('hop.hint')

vim.keymap.set('n', 'f',
  function()
    hop.hint_char1({
      direction = hop_hint.HintDirection.AFTER_CURSOR,
      current_line_only = true
    })
  end)
vim.keymap.set('n', 'F',
  function()
    hop.hint_char1({
      direction = hop_hint.HintDirection.BEFORE_CURSOOR,
      current_line_only = true
    })
  end)
vim.keymap.set('n', 't',
  function()
    hop.hint_char1({
      direction = hop_hint.HintDirection.AFTER_CURSOR,
      current_line_only = true,
      hint_offset = -1
    })
  end)
vim.keymap.set('n', 'T',
  function()
    hop.hint_char1({
      direction = hop_hint.HintDirection.BEFORE_CURSOR,
      current_line_only = true,
      hint_offset = 1
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
