local pack = require 'rchrand.pack'

pack.add {
  { src = pack.gh 'epwalsh/pomo.nvim', version = vim.version.range '*' },
  pack.gh 'rcarriga/nvim-notify',
  pack.gh 'folke/which-key.nvim',
}

require('pomo').setup {
  notifiers = {
    {
      name = 'Default',
      opts = {
        sticky = false,
      },
    },
    { name = 'System' },
  },
}

vim.keymap.set('n', '<leader>pp', '<cmd>TimerStart 25m Work<cr>', { desc = 'Start Pomodoro' })
vim.keymap.set('n', '<leader>ps', '<cmd>TimerShow<cr>', { desc = 'Show Pomodoro Status' })
vim.keymap.set('n', '<leader>ph', '<cmd>TimerHide<cr>', { desc = 'Hide Pomodoro Status' })
vim.keymap.set('n', '<leader>pq', '<cmd>TimerStop<cr>', { desc = 'Stop Pomodoro' })
require('which-key').add { { '<leader>p', group = '[P]omodoro' } }
