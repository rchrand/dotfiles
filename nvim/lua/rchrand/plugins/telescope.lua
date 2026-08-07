local pack = require 'rchrand.pack'

pack.add {
  pack.gh 'nvim-lua/plenary.nvim',
  pack.gh 'nvim-telescope/telescope.nvim',
  pack.gh 'nvim-telescope/telescope-ui-select.nvim',
  pack.gh 'nvim-telescope/telescope-file-browser.nvim',
}

if vim.fn.executable 'make' == 1 then
  pack.add { pack.gh 'nvim-telescope/telescope-fzf-native.nvim' }

  local fzf = vim.pack.get({ 'telescope-fzf-native.nvim' }, { info = false })[1]
  if fzf and not vim.uv.fs_stat(fzf.path .. '/build/libfzf.so') then
    pack.build('telescope-fzf-native.nvim', { 'make' })
  end
end

if vim.g.have_nerd_font then
  pack.add { pack.gh 'nvim-tree/nvim-web-devicons' }
end

local actions = require 'telescope.actions'

local function ts_select_dir_for_grep()
  local action_state = require 'telescope.actions.state'
  local fb = require('telescope').extensions.file_browser
  local live_grep = require('telescope.builtin').live_grep
  local current_line = action_state.get_current_line()

  fb.file_browser {
    files = false,
    depth = false,
    attach_mappings = function()
      actions.select_default:replace(function()
        local entry_path = action_state.get_selected_entry().Path
        local dir = entry_path:is_dir() and entry_path or entry_path:parent()
        local relative = dir:make_relative(vim.fn.getcwd())
        local absolute = dir:absolute()

        live_grep {
          results_title = relative .. '/',
          cwd = absolute,
          default_text = current_line,
        }
      end)

      return true
    end,
  }
end

require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-k>'] = actions.move_selection_previous,
        ['<C-j>'] = actions.move_selection_next,
        ['<C-p>'] = actions.move_selection_previous,
        ['<C-n>'] = actions.move_selection_next,
        ['<C-l>'] = actions.select_default,
        ['<C-u>'] = actions.preview_scrolling_up,
        ['<C-d>'] = actions.preview_scrolling_down,
      },
    },
    layout_strategy = 'horizontal',
    layout_config = {
      horizontal = {
        width = 0.95,
        height = 0.9,
        preview_width = 0.55,
      },
    },
  },
  pickers = {
    find_files = {
      file_ignore_patterns = { 'node_modules', '^.git/', '.venv' },
      hidden = true,
    },
    live_grep = {
      mappings = {
        i = { ['<C-f>'] = ts_select_dir_for_grep },
        n = { ['<C-f>'] = ts_select_dir_for_grep },
      },
      file_ignore_patterns = { 'node_modules', '^.git/', '.venv' },
      additional_args = function()
        return { '--hidden' }
      end,
    },
  },
  extensions = {
    ['ui-select'] = {
      require('telescope.themes').get_dropdown(),
    },
  },
}

pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'ui-select')
pcall(require('telescope').load_extension, 'file_browser')

local builtin = require 'telescope.builtin'
vim.keymap.set('n', '<leader>sh', builtin.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sk', builtin.keymaps, { desc = '[S]earch [K]eymaps' })
vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>ss', builtin.builtin, { desc = '[S]earch [S]elect Telescope' })
vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', builtin.resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>s.', builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
vim.keymap.set('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })

vim.keymap.set('n', '<leader>/', function()
  builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer' })

vim.keymap.set('n', '<leader>s/', function()
  builtin.live_grep {
    grep_open_files = true,
    prompt_title = 'Live Grep in Open Files',
  }
end, { desc = '[S]earch [/] in Open Files' })

vim.keymap.set('n', '<leader>sn', function()
  builtin.find_files { cwd = vim.fn.stdpath 'config' }
end, { desc = '[S]earch [N]eovim files' })
