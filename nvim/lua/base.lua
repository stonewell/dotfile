vim.cmd("autocmd!")

vim.scriptencoding = 'utf-8'

vim.opt.termencoding = 'utf-8'
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.fileformats = 'unix,dos,mac'
vim.opt.wrap = true
vim.opt.bomb = false
vim.opt.autochdir = true
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.viminfo = ''
vim.opt.filetype = 'on'
vim.opt.title = true
vim.opt.autoindent = true
vim.opt.smartindent = false
vim.opt.hlsearch = true
vim.opt.ignorecase = true -- Case insensitive searching UNLESS /C or capital in search
vim.opt.smarttab = true
vim.opt.breakindent = true
vim.opt.backspace = { 'start', 'eol', 'indent' }
vim.opt.path:append { '**' } -- Finding files - Search down into subfolders
vim.opt.wildignore:append { '*/node_modules/*' }

vim.opt.clipboard:append { 'unnamedplus' }

-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = '*',
  command = "set nopaste"
})

-- Seems no need to call the function for nvim-qt
-- but leave here for reference
vim.api.nvim_create_autocmd("UIEnter", {
  pattern = '*',
  callback = function()
    local chan_id = vim.v.event.chan

    if vim.v.event.chan > 0 then
      if vim.fn.has('GuiClipboard') then
	vim.defer_fn(function()
	  local chan_info = vim.api.nvim_get_chan_info(chan_id)

	  if chan_info.client.name == 'nvim-qt' then
	    vim.cmd 'call GuiClipboard()'
	  end
	end, 2200)
      end
    end
  end
})

-- Add asterisks in block comments
vim.opt.formatoptions:append { 'r' }
