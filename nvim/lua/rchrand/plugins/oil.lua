local pack = require 'rchrand.pack'

pack.add {
  pack.gh 'stevearc/oil.nvim',
  pack.gh 'echasnovski/mini.icons',
}

require('oil').setup {
  default_file_explorer = true,
  columns = {
    'icon',
  },
  buf_options = {
    buflisted = false,
    bufhidden = 'hide',
  },
  win_options = {
    wrap = false,
    signcolumn = 'no',
    cursorcolumn = false,
    foldcolumn = '0',
    spell = false,
    list = false,
    conceallevel = 3,
    concealcursor = 'nvic',
  },
  delete_to_trash = false,
  skip_confirm_for_simple_edits = false,
  prompt_save_on_select_new_entry = true,
  cleanup_delay_ms = 2000,
  lsp_file_methods = {
    enabled = true,
    timeout_ms = 1000,
    autosave_changes = false,
  },
  constrain_cursor = 'editable',
  watch_for_changes = false,
  keymaps = {
    ['g?'] = { 'actions.show_help', mode = 'n' },
    ['<CR>'] = 'actions.select',
    ['<C-s>'] = { 'actions.select', opts = { vertical = true } },
    ['<C-h>'] = { 'actions.select', opts = { horizontal = true } },
    ['<C-t>'] = { 'actions.select', opts = { tab = true } },
    ['<C-c>'] = { 'actions.close', mode = 'n' },
    ['<C-l>'] = 'actions.refresh',
    ['-'] = { 'actions.parent', mode = 'n' },
    ['_'] = { 'actions.open_cwd', mode = 'n' },
    ['`'] = { 'actions.cd', mode = 'n' },
    ['g~'] = { 'actions.cd', opts = { scope = 'tab' }, mode = 'n' },
    ['gs'] = { 'actions.change_sort', mode = 'n' },
    ['gx'] = 'actions.open_external',
    ['g.'] = { 'actions.toggle_hidden', mode = 'n' },
    ['g\\'] = { 'actions.toggle_trash', mode = 'n' },
    ['gp'] = 'actions.preview',
  },
  use_default_keymaps = false,
  view_options = {
    show_hidden = false,
    is_hidden_file = function(name)
      return name:match '^%.' ~= nil
    end,
    is_always_hidden = function()
      return false
    end,
    natural_order = 'fast',
    case_insensitive = false,
    sort = {
      { 'type', 'asc' },
      { 'name', 'asc' },
    },
  },
  float = {
    padding = 2,
    max_width = 0,
    max_height = 0,
    win_options = {
      winblend = 0,
    },
  },
  preview_win = {
    update_on_cursor_moved = true,
    preview_method = 'fast_scratch',
    disable_preview = function()
      return false
    end,
    win_options = {},
  },
}

vim.keymap.set('n', '-', '<CMD>Oil<CR>', { desc = 'Open parent directory' })
