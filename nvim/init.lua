local uv = vim.uv or vim.loop
is_win = uv.os_uname().sysname == 'Windows_NT'
is_mac = uv.os_uname().sysname == 'Darwin'
vim.tbl_islist = vim.islist
vim.deprecate = function() end
-- vim.lsp.buf_get_clients was removed in Neovim 0.10; project.nvim still calls it.
if not vim.lsp.buf_get_clients then
  vim.lsp.buf_get_clients = function(bufnr)
    return vim.lsp.get_clients({ bufnr = bufnr })
  end
end

require('base')
require('plugins')

-- mapping setup must after plugins
require('mapping')
require('theme')

if is_mac then
  require('mac')
end
