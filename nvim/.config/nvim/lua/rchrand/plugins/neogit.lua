local pack = require 'rchrand.pack'

pack.add {
  pack.gh 'NeogitOrg/neogit',
  pack.gh 'nvim-lua/plenary.nvim',
  pack.gh 'sindrets/diffview.nvim',
  pack.gh 'nvim-telescope/telescope.nvim',
}

require('neogit').setup {}
vim.keymap.set('n', '<Leader>g', ':Neogit<CR>', { desc = 'Open Neogit' })
