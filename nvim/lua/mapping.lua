local opts = { noremap = true, silent = true }

--Remap space as leader key
vim.keymap.set("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Navigate in insert mode
vim.keymap.set('i', '<C-f>',
  function()
    vim.cmd('normal! l')
  end)
vim.keymap.set('i', '<C-b>',
  function()
    vim.cmd('normal! h')
  end)
vim.keymap.set('i', '<C-n>',
  function()
    vim.cmd('normal! j')
  end)
vim.keymap.set('i', '<C-p>',
  function()
    vim.cmd('normal! k')
  end)
vim.keymap.set('i', '<C-a>',
  function()
    vim.cmd('normal! ^')
  end)
vim.keymap.set('i', '<C-e>', '<C-o>$')
vim.keymap.set('i', '<C-d>',
  function()
    vim.cmd('normal! x')
  end)

vim.keymap.set('n', '<C-a>', 'gg<S-v>G')

local status, hop = pcall(require, "hop")
if (status) then
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
    end, {desc="Show Hint for Last Search Pattern"})
end


local status, yanky = pcall(require, "yanky")
if (status) then
  vim.keymap.set({"n","x"}, "p", "<Plug>(YankyPutAfter)")
  vim.keymap.set({"n","x"}, "P", "<Plug>(YankyPutBefore)")
  vim.keymap.set({"n","x"}, "gp", "<Plug>(YankyGPutAfter)")
  vim.keymap.set({"n","x"}, "gP", "<Plug>(YankyGPutBefore)")

  vim.keymap.set("n", "<M-y>", "<Plug>(YankyCycleForward)")
end

