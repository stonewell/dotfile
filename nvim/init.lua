is_win = vim.loop.os_uname().sysname == 'Windows_NT'
is_mac = vim.loop.os_uname().sysname == 'Darwin'

require('impatient')

require('base')
require('plugins')

-- mapping setup must after plugins
require('mapping')
require('theme')

if is_mac then
  require('mac')
end
