local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd [[packadd packer.nvim]]
end

vim.cmd "autocmd BufWritePost plugins.lua source <afile> | PackerCompile"

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use 'nvim-lua/plenary.nvim'

  use 'equalsraf/neovim-gui-shim'

  use 'nvim-treesitter/nvim-treesitter'

  use 'bronson/vim-crosshairs'
  use 'editorconfig/editorconfig-vim'

  use {
    'nvim-lualine/lualine.nvim',
    config = function()
      require('lualine').setup()
    end
  }

  use 'kyazdani42/nvim-web-devicons'
  use {
    'karb94/neoscroll.nvim',
    config = function()
      require('neoscroll').setup()
    end
  }

  use {
    'ahmedkhalf/project.nvim',
    config = function()
      require('project_nvim').setup({

      })
    end
  }

  use {
    'phaazon/hop.nvim',
    config = function()
      require('hop').setup()
    end
  }

  use 'kyazdani42/nvim-tree.lua'
  use 'akinsho/nvim-bufferline.lua'

  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-file-browser.nvim'

  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make'
  }

  use 'windwp/nvim-autopairs'
  use 'windwp/nvim-ts-autotag'

  -- themes
  use 'norcalli/nvim-colorizer.lua'

  use {
    'ayu-theme/ayu-vim',
    config = function()
      vim.g.ayucolor = 'mirage'
      vim.cmd 'colorscheme ayu'
    end,
  }

  use 'kvrohit/rasmus.nvim'

  -- LSP
  use 'neovim/nvim-lspconfig' -- LSP
  use 'jose-elias-alvarez/null-ls.nvim' -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
  use 'MunifTanjim/prettier.nvim' -- Prettier plugin for Neovim's built-in LSP client

  use 'glepnir/lspsaga.nvim' -- LSP UIs

  -- yank ring UI
  use {
    'gbprod/yanky.nvim',
    config = function()
      local utils = require("yanky.utils")
      local mapping = require("yanky.telescope.mapping")

      require("yanky").setup({
	picker = {
    	  telescope = {
            mappings = {
              default = mapping.put("p"),
              i = {
                ["<c-j>"] = mapping.put("p"),
                ["<c-k>"] = mapping.put("P"),
                ["<c-x>"] = mapping.delete(),
                ["<c-r>"] = mapping.set_register(utils.get_default_register()),
              },
              n = {
                p = mapping.put("p"),
                P = mapping.put("P"),
                d = mapping.delete(),
                r = mapping.set_register(utils.get_default_register())
              },
            }
          }
        }
      })
    end,
  }

  -- Guess indent
  use 'tpope/vim-sleuth'

  -- Move the range instead of copy paste
  use 'matze/vim-move'

  -- incremental search enhance
  use 'haya14busa/is.vim'

  -- show current indent
  use {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      require('indent_blankline').setup({
        show_current_context = true,
        show_current_context_start = true,
      })
    end
  }

  -- deal with trail white space
  use {
    'ntpeters/vim-better-whitespace',
    config = function()
      vim.g.strip_only_modified_lines = 1
      vim.g.strip_whitespace_on_save = 1
    end
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end

end)

