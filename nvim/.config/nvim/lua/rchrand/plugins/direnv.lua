local pack = require 'rchrand.pack'

pack.add { pack.gh 'direnv/direnv.vim' }

if vim.fn.executable 'direnv' == 1 then
  pcall(vim.cmd.packadd, 'direnv.vim')
end
