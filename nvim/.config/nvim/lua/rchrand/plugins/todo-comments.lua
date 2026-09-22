local pack = require 'rchrand.pack'

pack.add {
  pack.gh 'folke/todo-comments.nvim',
  pack.gh 'nvim-lua/plenary.nvim',
}

require('todo-comments').setup { signs = false }
