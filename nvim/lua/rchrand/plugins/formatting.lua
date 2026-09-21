local pack = require 'rchrand.pack'

pack.add { pack.gh 'stevearc/conform.nvim' }

local local_formatter = vim.fn.expand '~/.config/rchrand/nvim/formatting.lua'
if vim.uv.fs_stat(local_formatter) then
  return dofile(local_formatter)
end

require('conform').setup {
  notify_on_error = false,
  format_on_save = function(bufnr)
    if ({ c = true, cpp = true })[vim.bo[bufnr].filetype] then
      return nil
    end
    return { timeout_ms = 1000, lsp_format = 'fallback' }
  end,
  formatters_by_ft = {
    lua = { 'stylua' },
    python = { 'ruff_fix', 'ruff_format' },
  },
}

vim.keymap.set({ 'n', 'v' }, '<leader>f', function()
  require('conform').format { async = true, lsp_format = 'fallback' }
end, { desc = '[F]ormat buffer' })
