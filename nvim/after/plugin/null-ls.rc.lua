local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettierd,
    null_ls.builtins.formatting.yapf,
    null_ls.builtins.formatting.clang_format,
    null_ls.builtins.diagnostics.eslint_d,
    null_ls.builtins.diagnostics.pylint,
    null_ls.builtins.diagnostics.write_good,
    null_ls.builtins.completion.spell,
    null_ls.builtins.formatting.uncrustify,
  },

  on_attach = function(client, bufnr)
    local opts = { noremap = true, silent = true }

    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_buf_set_keymap(bufnr, 'n', 'bf', '<cmd>lua vim.lsp.buf.format({ bufnr = bufnr })<CR>', opts)
    end
  end,
})
