local pack = require 'rchrand.pack'

pack.add { pack.gh 'lewis6991/gitsigns.nvim' }

require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
    untracked = { text = '┆' },
  },
  signcolumn = true,
  numhl = false,
  linehl = false,
  word_diff = false,
  attach_to_untracked = true,
  current_line_blame = false,
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = 'eol',
    delay = 1000,
    ignore_whitespace = false,
    virt_text_priority = 100,
  },
  current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
  sign_priority = 6,
  update_debounce = 100,
  max_file_length = 40000,
  preview_config = {
    border = 'single',
    style = 'minimal',
    relative = 'cursor',
    row = 0,
    col = 1,
  },
  on_attach = function(bufnr)
    local gitsigns = require 'gitsigns'

    local function map(mode, lhs, rhs, desc, opts)
      opts = opts or {}
      opts.buffer = bufnr
      opts.desc = desc
      vim.keymap.set(mode, lhs, rhs, opts)
    end

    map('n', ']c', function()
      if vim.wo.diff then
        return ']c'
      end
      vim.schedule(gitsigns.next_hunk)
      return '<Ignore>'
    end, 'Next git hunk', { expr = true })

    map('n', '[c', function()
      if vim.wo.diff then
        return '[c'
      end
      vim.schedule(gitsigns.prev_hunk)
      return '<Ignore>'
    end, 'Previous git hunk', { expr = true })

    map('n', '<leader>hp', gitsigns.preview_hunk, 'Preview git hunk')
    map('n', '<leader>hb', function()
      gitsigns.blame_line { full = true }
    end, 'Blame git line')
    map('n', '<leader>hd', gitsigns.diffthis, 'Diff git file')
    map('n', '<leader>hD', function()
      gitsigns.diffthis '~'
    end, 'Diff git file against base')
    map('n', '<leader>tb', gitsigns.toggle_current_line_blame, 'Toggle git blame')
    map('n', '<leader>td', gitsigns.toggle_deleted, 'Toggle deleted git lines')
    map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', 'Select git hunk')
  end,
}
