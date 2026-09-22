local pack = require 'rchrand.pack'

vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    local name = ev.data.spec.name
    local kind = ev.data.kind
    if name == 'fff.nvim' and (kind == 'install' or kind == 'update') then
      if not ev.data.active then
        vim.cmd.packadd 'fff.nvim'
      end

      require('fff.download').download_or_build_binary()
    end
  end,
})

pack.add { pack.gh 'dmtrKovalenko/fff.nvim' }

local fff_download = require 'fff.download'
local fff_binary = fff_download.get_binary_path()
local fff_binary_stat = vim.uv.fs_stat(fff_binary)

if not fff_binary_stat or fff_binary_stat.type ~= 'file' then
  fff_download.download_or_build_binary()
end

require('fff').setup {
  layout = {
    height = 0.95,
    width = 0.95,
    preview_position = 'top',
  },
  keymaps = {
    select = '<CR>',
    move_up = { '<C-k>', '<Up>' },
    move_down = { '<C-j>', '<Down>' },
    preview_scroll_up = '<C-u>',
    preview_scroll_down = '<C-d>',
    cycle_previous_query = '<C-p>',
  },
}

local rbi_exclude_query = '!*.rbi '

local function find_files_without_rbi(opts)
  opts = vim.tbl_deep_extend('force', { query = rbi_exclude_query }, opts or {})
  require('fff').find_files(opts)
end

vim.keymap.set('n', '<leader>sf', function()
  find_files_without_rbi()
end, { desc = '[S]earch [F]iles' })

vim.keymap.set('n', '<leader>sF', function()
  require('fff').find_files()
end, { desc = '[S]earch [F]iles including RBI' })

vim.keymap.set('n', '<C-p>', function()
  find_files_without_rbi()
end, { desc = '[ ] Search Files' })

vim.keymap.set('n', '<leader>sw', function()
  require('fff').live_grep { query = vim.fn.expand '<cword>' }
end, { desc = '[S]earch current [W]ord' })

vim.keymap.set('n', '<leader>sg', function()
  require('fff').live_grep()
end, { desc = '[S]earch by [G]rep' })

vim.keymap.set('n', '<leader>sn', function()
  find_files_without_rbi { title = 'Files in nvim', cwd = vim.fn.stdpath 'config' }
end, { desc = '[S]earch [N]eovim files' })
