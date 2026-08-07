local pack = require 'rchrand.pack'

pack.add { pack.gh 'stevearc/conform.nvim' }

require('conform').setup {
  notify_on_error = false,
  format_on_save = function(bufnr)
    local disable_filetypes = { c = true, cpp = true }
    if disable_filetypes[vim.bo[bufnr].filetype] then
      return nil
    end
    return {
      timeout_ms = 1000,
      lsp_format = 'fallback',
    }
  end,
  formatters_by_ft = {
    lua = { 'stylua' },
    python = { 'ruff_fix', 'ruff_format' },
  },
  formatters = (function()
    local landfolk_root = vim.fs.normalize(vim.fn.expand '~/landfolk/data')
    local landfolk_config = vim.fs.joinpath(landfolk_root, 'pyproject.toml')

    local function project_root(bufnr)
      local bufname = vim.api.nvim_buf_get_name(bufnr)
      local start = vim.fs.dirname(bufname)
      local root_file = vim.fs.find({ 'pyproject.toml', 'poetry.lock', 'setup.cfg', 'setup.py', 'requirements.txt', '.git' }, {
        path = start,
        upward = true,
      })[1]
      return root_file and vim.fs.dirname(root_file) or start
    end

    local function is_landfolk_buffer(bufnr)
      local bufname = vim.api.nvim_buf_get_name(bufnr)
      if bufname == '' then
        return false
      end

      local normalized = vim.fs.normalize(bufname)
      return normalized == landfolk_root or vim.startswith(normalized, landfolk_root .. '/')
    end

    local function ruff_cmd(bufnr)
      local root = project_root(bufnr)
      local function exe(path)
        return path and #path > 0 and vim.fn.executable(path) == 1
      end
      local function join(...)
        return vim.fs.joinpath(...)
      end

      for _, name in ipairs { '.venv', 'venv', 'env' } do
        local candidate = join(root, name, 'bin', 'ruff')
        if exe(candidate) then
          return candidate, root
        end
      end
      if vim.env.VIRTUAL_ENV and exe(join(vim.env.VIRTUAL_ENV, 'bin', 'ruff')) then
        return join(vim.env.VIRTUAL_ENV, 'bin', 'ruff'), root
      end
      local fallback = join(landfolk_root, '.venv', 'bin', 'ruff')
      if is_landfolk_buffer(bufnr) and exe(fallback) then
        return fallback, root
      end
      local mason = join(vim.fn.stdpath 'data', 'mason', 'bin', 'ruff')
      if exe(mason) then
        return mason, root
      end
      return 'ruff', root
    end

    local function ruff_args(bufnr, subcommand)
      local args = { subcommand, '--force-exclude' }

      if subcommand == 'check' then
        vim.list_extend(args, { '--fix', '--exit-zero', '--no-cache' })
      end

      if is_landfolk_buffer(bufnr) and vim.uv.fs_stat(landfolk_config) then
        vim.list_extend(args, { '--config', landfolk_config })
      end

      vim.list_extend(args, { '--stdin-filename', '$FILENAME', '-' })
      return args
    end

    return {
      ruff_fix = {
        command = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return ruff_cmd(bufnr)
        end,
        args = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return ruff_args(bufnr, 'check')
        end,
        stdin = true,
        cwd = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return project_root(bufnr)
        end,
      },
      ruff_format = {
        command = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return ruff_cmd(bufnr)
        end,
        args = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return ruff_args(bufnr, 'format')
        end,
        stdin = true,
        cwd = function(_, ctx)
          local bufnr = ctx and ctx.buf or vim.api.nvim_get_current_buf()
          return project_root(bufnr)
        end,
      },
    }
  end)(),
}

vim.keymap.set({ 'n', 'v' }, '<leader>f', function()
  require('conform').format { async = true, lsp_format = 'fallback' }
end, { desc = '[F]ormat buffer' })
