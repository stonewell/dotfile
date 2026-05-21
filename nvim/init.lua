local uv = vim.uv or vim.loop
is_win = uv.os_uname().sysname == 'Windows_NT'
is_mac = uv.os_uname().sysname == 'Darwin'
vim.tbl_islist = vim.islist
vim.deprecate = function() end

require('base')
require('plugins')

-- mapping setup must after plugins
require('mapping')
require('theme')

if is_mac then
  require('mac')
end
