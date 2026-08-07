local pack = require 'rchrand.pack'

pack.add {
  pack.gh 'neovim/nvim-lspconfig',
  pack.gh 'mason-org/mason.nvim',
  pack.gh 'WhoIsSethDaniel/mason-tool-installer.nvim',
  pack.gh 'j-hui/fidget.nvim',
}

require('mason').setup()
require('fidget').setup {}

local function raylib_compile_flags(opts)
  local root = vim.fs.root(0, { 'compile_commands.json', 'compile_flags.txt', '.clangd', 'CMakeLists.txt', 'Makefile', 'flake.nix', '.git' }) or vim.uv.cwd()
  local output = vim.fs.joinpath(root, 'compile_flags.txt')

  if opts.bang == false and vim.uv.fs_stat(output) then
    vim.notify('compile_flags.txt already exists; use :RaylibCompileFlags! to overwrite', vim.log.levels.WARN)
    return
  end

  local result = vim.system({ 'pkg-config', '--cflags', 'raylib' }, { text = true }):wait()
  if result.code ~= 0 then
    vim.notify('pkg-config --cflags raylib failed. Enter the project dev shell or allow direnv first.', vim.log.levels.ERROR)
    return
  end

  local flags = { '-std=c11', '-Wall', '-Wextra', '-pedantic' }
  vim.list_extend(flags, vim.split(vim.trim(result.stdout), '%s+', { trimempty = true }))
  vim.fn.writefile(flags, output)
  vim.notify('Wrote ' .. output .. '; restart clangd or reopen C files if diagnostics do not refresh')
end

vim.api.nvim_create_user_command('RaylibCompileFlags', raylib_compile_flags, {
  bang = true,
  desc = 'Write compile_flags.txt from pkg-config raylib flags',
})

local lsp_attach_group = vim.api.nvim_create_augroup('rchrand-lsp-attach', { clear = true })
local lsp_highlight_group = vim.api.nvim_create_augroup('rchrand-lsp-highlight', { clear = true })
local lsp_detach_group = vim.api.nvim_create_augroup('rchrand-lsp-detach', { clear = true })

vim.api.nvim_create_autocmd('LspAttach', {
  group = lsp_attach_group,
  callback = function(event)
    local map = function(keys, func, desc, mode)
      mode = mode or 'n'
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
    end

    map('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
    map('gW', require('telescope.builtin').lsp_dynamic_workspace_symbols, 'Open Workspace Symbols')

    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) and not vim.b[event.buf].rchrand_lsp_highlight then
      vim.b[event.buf].rchrand_lsp_highlight = true
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        buffer = event.buf,
        group = lsp_highlight_group,
        callback = vim.lsp.buf.document_highlight,
      })

      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        buffer = event.buf,
        group = lsp_highlight_group,
        callback = vim.lsp.buf.clear_references,
      })
    end

    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
      map('<leader>th', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
      end, '[T]oggle Inlay [H]ints')
    end

    if client and client.name == 'clangd' then
      map('<leader>ch', function()
        vim.cmd.LspClangdSwitchSourceHeader()
      end, 'Switch [C] source/[H]eader')
    end
  end,
})

vim.api.nvim_create_autocmd('LspDetach', {
  group = lsp_detach_group,
  callback = function(event)
    vim.schedule(function()
      if not vim.api.nvim_buf_is_valid(event.buf) then
        return
      end

      for _, client in ipairs(vim.lsp.get_clients { bufnr = event.buf }) do
        if client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
          return
        end
      end

      vim.api.nvim_buf_call(event.buf, vim.lsp.buf.clear_references)
      vim.api.nvim_clear_autocmds { group = lsp_highlight_group, buffer = event.buf }
      vim.b[event.buf].rchrand_lsp_highlight = false
    end)
  end,
})

vim.diagnostic.config {
  severity_sort = true,
  float = { source = 'if_many' },
  underline = { severity = vim.diagnostic.severity.ERROR },
  signs = vim.g.have_nerd_font and {
    text = {
      [vim.diagnostic.severity.ERROR] = '󰅚 ',
      [vim.diagnostic.severity.WARN] = '󰀪 ',
      [vim.diagnostic.severity.INFO] = '󰋽 ',
      [vim.diagnostic.severity.HINT] = '󰌶 ',
    },
  } or {},
  virtual_text = {
    source = 'if_many',
    spacing = 2,
    format = function(diagnostic)
      return diagnostic.message
    end,
  },
}

local capabilities = require('blink.cmp').get_lsp_capabilities()
capabilities.general = capabilities.general or {}
capabilities.general.positionEncodings = { 'utf-8' }

local command_cache = {}
local direnv_block_notified = {}

local function project_root(bufnr, markers)
  return vim.fs.root(bufnr, markers)
end

local function has_envrc(root)
  return root and vim.uv.fs_stat(vim.fs.joinpath(root, '.envrc')) ~= nil
end

local function asdf_gem_env(root)
  local result = vim.system({ 'asdf', 'exec', 'ruby', '-e', 'print Gem.dir' }, { cwd = root, text = true }):wait()
  if result.code ~= 0 then
    return nil
  end

  local gem_dir = vim.trim(result.stdout)
  return { GEM_HOME = gem_dir, GEM_PATH = gem_dir }
end

local function command_result(root, key, command)
  local cache_key = table.concat({ root or '', key }, '\n')
  if command_cache[cache_key] ~= nil then
    return command_cache[cache_key]
  end

  local opts = { cwd = root, text = true }
  if command[1] == 'asdf' then
    opts.env = asdf_gem_env(root)
  end
  local result = vim.system(command, opts):wait()
  command_cache[cache_key] = result.code == 0
  return command_cache[cache_key]
end

local function command_source(root, executable)
  local cache_key = table.concat({ root or '', executable, 'source' }, '\n')
  if command_cache[cache_key] ~= nil then
    return command_cache[cache_key]
  end

  if has_envrc(root) and vim.fn.executable 'direnv' == 1 then
    local direnv_key = table.concat({ root or '', executable, 'direnv' }, '\n')
    local direnv_result = vim.system({ 'direnv', 'exec', root, 'sh', '-c', 'command -v ' .. executable }, { text = true }):wait()
    command_cache[direnv_key] = direnv_result.code == 0

    if command_cache[direnv_key] then
      command_cache[cache_key] = 'direnv'
      return 'direnv'
    end

    if root and not direnv_block_notified[root] and (direnv_result.stderr or ''):match 'is blocked' then
      direnv_block_notified[root] = true
      vim.notify(('direnv is blocked for %s; run: direnv allow %s'):format(root, root), vim.log.levels.WARN)
    end
  end

  if root and vim.fn.executable 'asdf' == 1 then
    local asdf_key = table.concat({ root, executable, 'asdf' }, '\n')
    if command_cache[asdf_key] == nil then
      local asdf_result = vim.system({ 'asdf', 'which', executable }, { cwd = root, text = true }):wait()
      command_cache[asdf_key] = asdf_result.code == 0
    end

    if command_cache[asdf_key] then
      command_cache[cache_key] = 'asdf'
      return 'asdf'
    end
  end

  if vim.fn.executable(executable) == 1 then
    command_cache[cache_key] = 'path'
    return 'path'
  end

  command_cache[cache_key] = false
  return false
end

local function tool_root_dir(markers, executable, predicate)
  return function(bufnr, on_dir)
    local root = project_root(bufnr, markers)
    if not root then
      return
    end
    if predicate and not predicate(root) then
      return
    end
    if not command_source(root, executable) then
      return
    end
    on_dir(root)
  end
end

local function tool_cmd(executable, args)
  args = args or {}
  return function(dispatchers, config)
    local root = config and config.root_dir or vim.uv.cwd()
    local source = command_source(root, executable)
    local cmd = {}
    if source == 'direnv' then
      cmd = { 'direnv', 'exec', root, executable }
    elseif source == 'asdf' then
      cmd = { 'asdf', 'exec', executable }
    else
      cmd = { executable }
    end
    vim.list_extend(cmd, args)
    local opts = config and config.root_dir and { cwd = config.root_dir } or {}
    if source == 'asdf' then
      opts.env = asdf_gem_env(root)
    end
    return vim.lsp.rpc.start(cmd, dispatchers, opts)
  end
end

local function file_contains(path, pattern)
  local lines = vim.fn.readfile(path)
  for _, line in ipairs(lines) do
    if line:match(pattern) then
      return true
    end
  end
  return false
end

local function has_sorbet(root)
  local lockfile = vim.fs.joinpath(root, 'Gemfile.lock')
  return vim.uv.fs_stat(lockfile) and file_contains(lockfile, '^    sorbet %(')
end

local function sorbet_available(root)
  if not has_sorbet(root) then
    return false
  end

  local cmd = { 'bundle', 'exec', 'srb', '--version' }
  local source = command_source(root, 'bundle')
  if source == 'direnv' then
    cmd = { 'direnv', 'exec', root, 'bundle', 'exec', 'srb', '--version' }
  elseif source == 'asdf' then
    cmd = { 'asdf', 'exec', 'bundle', 'exec', 'srb', '--version' }
  end
  return command_result(root, 'bundle-exec-srb', cmd)
end

local function sorbet_cmd(dispatchers, config)
  local root = config and config.root_dir or vim.uv.cwd()
  local cmd = { 'bundle', 'exec', 'srb', 'tc', '--lsp', '--disable-watchman' }
  local source = command_source(root, 'bundle')
  if source == 'direnv' then
    cmd = { 'direnv', 'exec', root, 'bundle', 'exec', 'srb', 'tc', '--lsp', '--disable-watchman' }
  elseif source == 'asdf' then
    cmd = { 'asdf', 'exec', 'bundle', 'exec', 'srb', 'tc', '--lsp', '--disable-watchman' }
  end
  local opts = config and config.root_dir and { cwd = config.root_dir } or {}
  if source == 'asdf' then
    opts.env = asdf_gem_env(root)
  end
  return vim.lsp.rpc.start(cmd, dispatchers, opts)
end

if vim.g.rchrand_enable_ty == nil then
  vim.g.rchrand_enable_ty = true
end
if vim.g.rchrand_enable_ruff == nil then
  vim.g.rchrand_enable_ruff = true
end

local servers = {
  ty = {
    cmd = tool_cmd('ty', { 'server' }),
    filetypes = { 'python' },
    root_dir = tool_root_dir({ 'pyproject.toml', 'ty.toml', '.git' }, 'ty'),
    settings = {
      ty = {
        diagnosticMode = 'openFilesOnly',
      },
    },
  },
  ruff = {
    cmd = tool_cmd('ruff', { 'server' }),
    root_dir = tool_root_dir({ 'pyproject.toml', 'ruff.toml', '.ruff.toml', '.git' }, 'ruff'),
    init_options = {
      settings = {
        organizeImports = true,
      },
    },
  },
  lua_ls = {
    settings = {
      Lua = {
        completion = {
          callSnippet = 'Replace',
        },
      },
    },
  },
  ruby_lsp = {
    cmd = tool_cmd 'ruby-lsp',
    root_dir = tool_root_dir({ 'Gemfile', '.ruby-lsp', '.git' }, 'ruby-lsp'),
  },
  sorbet = {
    cmd = sorbet_cmd,
    filetypes = { 'ruby' },
    root_dir = function(bufnr, on_dir)
      local root = project_root(bufnr, { 'Gemfile', '.git' })
      if root and sorbet_available(root) then
        on_dir(root)
      end
    end,
  },
  clangd = {
    cmd = tool_cmd 'clangd',
    root_dir = tool_root_dir({
      '.clangd',
      '.clang-tidy',
      '.clang-format',
      'compile_commands.json',
      'compile_flags.txt',
      'CMakeLists.txt',
      'Makefile',
      'flake.nix',
      '.git',
    }, 'clangd'),
    root_markers = {
      '.clangd',
      '.clang-tidy',
      '.clang-format',
      'compile_commands.json',
      'compile_flags.txt',
      'CMakeLists.txt',
      'Makefile',
      'flake.nix',
      '.git',
    },
  },
}

vim.lsp.config('*', { capabilities = capabilities })

for server_name, server in pairs(servers) do
  vim.lsp.config(server_name, server)
end

local function server_enabled(server_name)
  if server_name == 'ty' then
    return vim.g.rchrand_enable_ty
  end
  if server_name == 'ruff' then
    return vim.g.rchrand_enable_ruff
  end
  return true
end

local function enabled_servers()
  return vim
    .iter(vim.tbl_keys(servers))
    :filter(function(server_name)
      return server_enabled(server_name)
    end)
    :totable()
end

local function toggle_server(server_name, global_name)
  vim.g[global_name] = not vim.g[global_name]
  vim.lsp.enable(server_name, vim.g[global_name])
  vim.notify(('%s LSP %s'):format(server_name, vim.g[global_name] and 'enabled' or 'disabled'))
end

vim.api.nvim_create_user_command('TyToggle', function()
  toggle_server('ty', 'rchrand_enable_ty')
end, { desc = 'Toggle ty LSP' })

vim.api.nvim_create_user_command('RuffToggle', function()
  toggle_server('ruff', 'rchrand_enable_ruff')
end, { desc = 'Toggle ruff LSP' })

require('mason-tool-installer').setup {
  ensure_installed = { 'ty', 'ruff', 'lua-language-server', 'stylua', 'tree-sitter-cli' },
}

vim.lsp.enable(enabled_servers())
