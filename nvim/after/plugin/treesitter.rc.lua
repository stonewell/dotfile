if is_win then
  return
end

local status, ts = pcall(require, "nvim-treesitter.configs")
if (not status) then return end

vim.cmd ':TSUpdate'

ts.setup {
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = {
    "bash",
    "c",
    "cpp",
    "cmake",
    "dockerfile",
    "go",
    "html",
    "http",
    "javascript",
    "java",
    "json5",
    "jsonc",
    "json",
    "kotlin",
    "lua",
    "make",
    "markdown",
    "python",
    "regex",
    "rust",
    "toml",
    "typescript",
    "vim",
    "yaml",
  },

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  ignore_install = { },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = { },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },

  indent = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = { },
  },

  autotag = {
    -- `false` will disable the whole extension
    enable = true,
  },
}
