local pack = require 'rchrand.pack'

pack.add { pack.gh 'nvim-treesitter/nvim-treesitter' }

local treesitter = require 'nvim-treesitter'
local parsers = {
  'bash',
  'c',
  'diff',
  'dockerfile',
  'html',
  'lua',
  'luadoc',
  'markdown',
  'markdown_inline',
  'python',
  'query',
  'ruby',
  'sql',
  'toml',
  'vim',
  'vimdoc',
  'yaml',
  'zig',
}

treesitter.setup { install_dir = vim.fn.stdpath 'data' .. '/site' }
if vim.fn.executable 'tree-sitter' == 1 then
  treesitter.install(parsers)
end

vim.api.nvim_create_autocmd('FileType', {
  callback = function(args)
    local ok = pcall(vim.treesitter.start, args.buf)
    if not ok then
      return
    end

    if args.match == 'ruby' or args.match == 'sql' then
      vim.bo[args.buf].syntax = 'ON'
      return
    end

    vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
