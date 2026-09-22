local pack = require 'rchrand.pack'

pack.add { pack.gh 'rgroli/other.nvim' }

require('other-nvim').setup {
  mappings = {
    'python',
    'react',
    'elixir',
    {
      pattern = '/app/(.*)/(.*).rb$',
      target = '/spec/%1/%2_spec.rb',
      context = 'test',
    },
    {
      pattern = '/spec/(.*)/(.*)_spec.rb$',
      target = '/app/%1/%2.rb',
      context = 'source',
    },
  },
}

vim.keymap.set('n', '<leader>oo', '<cmd>Other<cr>', { desc = 'Open other file' })
