local M = {}

local function plugin_names()
  return vim
    .iter(vim.pack.get(nil, { info = false }))
    :map(function(plugin)
      return plugin.spec.name
    end)
    :totable()
end

local function command_names(opts)
  if opts.args == '' then
    return nil
  end
  return vim.split(opts.args, '%s+', { trimempty = true })
end

function M.add(specs)
  vim.pack.add(specs, { confirm = false })
end

function M.gh(repo)
  return 'https://github.com/' .. repo
end

function M.build(plugin_name, command)
  local plugin = vim.pack.get({ plugin_name }, { info = false })[1]
  if not plugin then
    return
  end

  local result = vim.system(command, { cwd = plugin.path }):wait()
  if result.code ~= 0 then
    vim.notify('Failed to build ' .. plugin_name, vim.log.levels.WARN)
  end
end

function M.setup_commands()
  vim.api.nvim_create_user_command('PackUpdate', function(opts)
    vim.pack.update(command_names(opts), { force = opts.bang })
  end, {
    bang = true,
    nargs = '*',
    complete = function()
      return plugin_names()
    end,
    desc = 'Update plugins with vim.pack',
  })

  vim.api.nvim_create_user_command('PackSync', function(opts)
    vim.pack.update(command_names(opts), { force = opts.bang, target = 'lockfile' })
  end, {
    bang = true,
    nargs = '*',
    complete = function()
      return plugin_names()
    end,
    desc = 'Sync plugins from nvim-pack-lock.json',
  })
end

M.setup_commands()

return M
