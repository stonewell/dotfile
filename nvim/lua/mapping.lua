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
vim.keymap.set('i', '<C-e>',
  function()
    vim.cmd('normal! A ')
  end)
vim.keymap.set('i', '<C-d>',
  function()
    vim.cmd('normal! x')
  end)

