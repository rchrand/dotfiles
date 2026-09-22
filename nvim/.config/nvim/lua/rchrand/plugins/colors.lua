local pack = require 'rchrand.pack'

pack.add {
  { src = pack.gh 'kepano/flexoki-neovim', name = 'flexoki' },
  { src = pack.gh 'marko-cerovac/material.nvim', name = 'material' },
}

if vim.g.rchrand_theme == 'flexoki-light' then
  vim.o.background = 'light'
  vim.cmd.colorscheme 'flexoki-light'
elseif vim.g.rchrand_theme == 'material-darker' then
  vim.o.background = 'dark'
  vim.g.material_style = 'darker'
  vim.cmd.colorscheme 'material'
end
