local pack = require 'rchrand.pack'

pack.add {
  { src = pack.gh 'saghen/blink.cmp', version = vim.version.range '1' },
  pack.gh 'folke/lazydev.nvim',
  pack.gh 'Bilal2453/luvit-meta',
}

require('lazydev').setup {
  library = {
    { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
  },
}

require('blink.cmp').setup {
  keymap = {
    preset = 'default',
  },
  appearance = {
    nerd_font_variant = 'mono',
  },
  completion = {
    documentation = { auto_show = false, auto_show_delay_ms = 500 },
  },
  sources = {
    default = { 'lsp', 'path', 'snippets', 'lazydev' },
    providers = {
      lazydev = { module = 'lazydev.integrations.blink', score_offset = 100 },
    },
  },
  signature = { enabled = true },
}
