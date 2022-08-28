local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.yapf,
    null_ls.builtins.diagnostics.eslint,
    null_ls.builtins.diagnostics.pylint,
    null_ls.builtins.diagnostics.write_good,
    null_ls.builtins.completion.spell,
    null_ls.builtins.formatting.uncrustify,
  },

  on_attach = function(client, bufnr)
    local opts = { noremap = true, silent = true }

    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_buf_set_keymap(bufnr, 'n', 'bf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
    end

    if client.server_capabilities.documentRangeFormattingProvider then
      vim.api.nvim_buf_set_keymap(bufnr, 'v', 'bf', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', opts)
    end
  end,
})

