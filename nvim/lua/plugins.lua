local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

return require('lazy').setup({
  'nvim-lua/plenary.nvim',

  {
    'kyazdani42/nvim-web-devicons',
    config = function()
      require('nvim-web-devicons').setup({
        default = true
      })
    end
  },

  {
    'ojroques/nvim-bufdel',
    opts = {},
    cmd = 'BufDel',
  },

  'equalsraf/neovim-gui-shim',

  'nvim-treesitter/nvim-treesitter',

  'bronson/vim-crosshairs',
  'editorconfig/editorconfig-vim',

  {
    'nvim-lualine/lualine.nvim',
    config = function()
      require('lualine').setup {
        options = {
          theme = 'dracula-nvim'
        }
      }
    end
  },

  {
    'karb94/neoscroll.nvim',
    config = function()
      require('neoscroll').setup()
    end
  },

  {
    'ahmedkhalf/project.nvim',
    config = function()
      require('project_nvim').setup({

      })
    end
  },

  {
    'phaazon/hop.nvim',
    config = function()
      require('hop').setup()
    end
  },

  'akinsho/nvim-bufferline.lua',

  'nvim-telescope/telescope.nvim',
  'nvim-telescope/telescope-file-browser.nvim',
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make'
  },

  {
    'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup({
        disable_filetype = { "TelescopePrompt" , "vim" },
      })
    end
  },

  {
    'windwp/nvim-ts-autotag',
    config = function()
      require('nvim-ts-autotag').setup()
    end
  },

  -- themes
  {
    'norcalli/nvim-colorizer.lua',
    config = function()
      require('colorizer').setup({
        '*';
      })
    end
  },

  {
    'Mofiqul/dracula.nvim',
    config = function()
      require('dracula').setup()
    end
  },

  -- LSP
  'neovim/nvim-lspconfig',
  'jose-elias-alvarez/null-ls.nvim',

  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {'williamboman/mason.nvim'},           -- Optional
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},         -- Required
      {'hrsh7th/cmp-nvim-lsp'},     -- Required
      {'hrsh7th/cmp-buffer'},       -- Optional
      {'hrsh7th/cmp-path'},         -- Optional
      {'saadparwaiz1/cmp_luasnip'}, -- Optional
      {'hrsh7th/cmp-nvim-lua'},     -- Optional

      -- Snippets
      {'L3MON4D3/LuaSnip'},             -- Required
      {'rafamadriz/friendly-snippets'}, -- Optional
    }
  },

  -- Prettier plugin for Neovim's built-in LSP client
  {
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
  },

  {
    'glepnir/lspsaga.nvim', -- LSP UIs
    event = "LspAttach",
    dependencies = {
      --Please make sure you install markdown and markdown_inline parser
      {"nvim-treesitter/nvim-treesitter"}
    }
  },

  -- yank ring UI
  'gbprod/yanky.nvim',

  -- Guess indent
  'tpope/vim-sleuth',

  -- Move the range instead of copy paste
  'matze/vim-move',

  -- incremental search enhance
  'haya14busa/is.vim',

  -- show current indent
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'VeryLazy',
    dependencies = 'nvim-treesitter',
    config = function()
      require('indent_blankline').setup({
        show_current_context = true,
        show_current_context_start = true,
      })
    end
  },

  -- deal with trail white space
  {
    'ntpeters/vim-better-whitespace',
    config = function()
      vim.g.strip_only_modified_lines = 1
      vim.g.strip_whitespace_on_save = 1
    end
  },

  -- remove search hl after cusor move or mode change
  {'romainl/vim-cool', event='VeryLazy' },

  -- improve startup time
  {
    'lewis6991/impatient.nvim',
  },

  {
    'folke/which-key.nvim',
    config = function()
      require('which-key').setup()
    end,
  },
})

