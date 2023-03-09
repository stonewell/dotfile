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

  use {
    'kyazdani42/nvim-web-devicons',
    config = function()
      require('nvim-web-devicons').setup({
        default = true
      })
    end
  }

  use 'equalsraf/neovim-gui-shim'

  use 'nvim-treesitter/nvim-treesitter'

  use 'bronson/vim-crosshairs'
  use 'editorconfig/editorconfig-vim'

  use {
    'nvim-lualine/lualine.nvim',
    config = function()
      require('lualine').setup {
        options = {
          theme = 'dracula-nvim'
        }
      }
    end
  }

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

  use 'akinsho/nvim-bufferline.lua'

  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-file-browser.nvim'
  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make'
  }

  use {
    'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup({
        disable_filetype = { "TelescopePrompt" , "vim" },
      })
    end
  }
  use {
    'windwp/nvim-ts-autotag',
    config = function()
      require('nvim-ts-autotag').setup()
    end
  }

  -- themes
  use {
    'norcalli/nvim-colorizer.lua',
    config = function()
      require('colorizer').setup({
        '*';
      })
    end
  }

  use {
    'Mofiqul/dracula.nvim',
    config = function()
      require('dracula').setup()
    end
  }

  -- LSP
  use 'neovim/nvim-lspconfig' -- LSP
  use 'jose-elias-alvarez/null-ls.nvim' -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua

  -- Prettier plugin for Neovim's built-in LSP client
  use {
    'MunifTanjim/prettier.nvim',
    config = function()
      require('prettier') .setup({
        bin = 'prettierd',
        filetypes = {
          "css",
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
          "json",
          "scss",
          "less"
        }
      })
    end
  }

  use 'glepnir/lspsaga.nvim' -- LSP UIs

  -- yank ring UI
  use 'gbprod/yanky.nvim'

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

  -- remove search hl after cusor move or mode change
  use 'romainl/vim-cool'

  -- improve startup time
  use {
    'lewis6991/impatient.nvim',
    config = function()
      require('impatient')
    end
  }

  use {
    'folke/which-key.nvim',
    config = function()
      require('which-key').setup()
    end
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end

end)

