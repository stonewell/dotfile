is_win = vim.loop.os_uname().sysname == 'Windows_NT'
is_mac = vim.loop.os_uname().sysname == 'Darwin'

require('base')
require('theme')
require('plugins')
require('mapping')

if is_mac then
  require('mac')
end
