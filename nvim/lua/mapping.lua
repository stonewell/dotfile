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

local status, yanky = pcall(require, "yanky")
if (status) then
  vim.keymap.set({"n","x"}, "p", "<Plug>(YankyPutAfter)")
  vim.keymap.set({"n","x"}, "P", "<Plug>(YankyPutBefore)")
  vim.keymap.set({"n","x"}, "gp", "<Plug>(YankyGPutAfter)")
  vim.keymap.set({"n","x"}, "gP", "<Plug>(YankyGPutBefore)")

  vim.keymap.set("n", "<M-y>", "<Plug>(YankyCycleForward)")
end

vim.keymap.set('n', '<leader>-', '<cmd>vsp<cr>')
vim.keymap.set('n', '<leader>=', '<cmd>split<cr>')

local status, minibufremove = pcall(require, "mini.bufremove")
if (status) then
  vim.keymap.set("n", "<leader>bd", function() minibufremove.delete(0, false) end, {desc = "Delete Buffer" })
  vim.keymap.set("n", "<leader>bD", function() minibufremove.delete(0, true) end, {desc = "Delete Buffer (Force)" })
end
