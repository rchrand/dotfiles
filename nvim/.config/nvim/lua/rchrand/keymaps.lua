-- [[ Basic Keymaps ]]
-- See `:help vim.keymap.set()`

vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })
vim.keymap.set('n', '<leader>u', '<cmd>Undotree<CR>', { desc = 'Open [U]ndo tree' })

vim.keymap.set('n', '<leader>tt', '<cmd>ThemeToggle<CR>', { desc = '[T]oggle [T]heme' })

vim.keymap.set('n', '<leader>rr', function()
  local root = vim.fs.root(0, 'Makefile') or vim.fn.getcwd()

  vim.cmd 'botright 12new'
  vim.fn.jobstart({ 'make', '-C', root, 'run' }, { term = true })
  vim.cmd 'startinsert'
end, { desc = '[R]un project' })

vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })
vim.keymap.set('n', '<Leader>wv', '<C-w>v', { desc = 'Create vertical split' })
vim.keymap.set('n', '<Leader>ws', '<C-w>s', { desc = 'Create horizontal split' })

vim.keymap.set('n', '<Leader>e', ':e #<CR>', { desc = 'Swap to previous buffer' })
