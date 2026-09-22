vim.api.nvim_create_autocmd('BufWritePre', {
  desc = 'Remove trailing whitespace from writable file buffers',
  group = vim.api.nvim_create_augroup('rchrand-trailing-whitespace', { clear = true }),
  callback = function(args)
    if vim.bo[args.buf].buftype ~= '' or not vim.bo[args.buf].modifiable then
      return
    end

    vim.api.nvim_buf_call(args.buf, function()
      vim.cmd [[keeppatterns %s/\s\+$//e]]
    end)
  end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('rchrand-highlight-yank', { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})
