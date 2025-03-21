local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettierd,
    null_ls.builtins.formatting.yapf,
    null_ls.builtins.formatting.clang_format,
    null_ls.builtins.diagnostics.pylint,
    null_ls.builtins.diagnostics.write_good,
    null_ls.builtins.completion.spell,
    null_ls.builtins.formatting.uncrustify,
    null_ls.builtins.diagnostics.luacheck,
    null_ls.builtins.formatting.stylua,
  },

  on_attach = function(client, bufnr)
    if client.server_capabilities.documentFormattingProvider then
      local opts = { noremap = true, silent = true, desc="Format Buffer", buffer=bufnr }

      vim.keymap.set('n', '<leader>f', function()
        vim.lsp.buf.format({ bufnr = bufnr, timeout_ms=50000 })
        vim.notify('buffer format done.')
      end, opts)
    end

    if client.server_capabilities.documentRangeFormattingProvider then
      local opts = { noremap = true, silent = true, desc="Format Selected Range", buffer=bufnr }

      vim.keymap.set('v', '<leader>f', function()
        vim.lsp.buf.format({ bufnr = bufnr, timeout_ms=50000 })
        vim.notify('range format done')
      end, opts)
    end

  end,
})
