local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettierd,
    null_ls.builtins.formatting.yapf,
    null_ls.builtins.formatting.clang_format,
    null_ls.builtins.diagnostics.eslint_d.with({ -- js/ts linter
      -- only enable eslint if root has .eslintrc.cjs
      condition = function(utils)
        return utils.root_has_file(".eslintrc.cjs") or utils.root_has_file(".eslintrc.json")
      end,
    }),
    null_ls.builtins.diagnostics.pylint,
    null_ls.builtins.diagnostics.write_good,
    null_ls.builtins.completion.spell,
    null_ls.builtins.formatting.uncrustify,
  },

  on_attach = function(client, bufnr)
    local opts = { noremap = true, silent = true }

    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_buf_set_keymap(bufnr, 'n', 'bf', '<cmd>lua vim.lsp.buf.format({ bufnr = bufnr, timeout_ms=50000 })<CR>', opts)
    end
  end,
})
