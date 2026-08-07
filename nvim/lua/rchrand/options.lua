-- [[ Setting options ]]
-- See `:help vim.o`

vim.o.number = true
-- vim.o.relativenumber = true

vim.o.mouse = 'a'
vim.o.showmode = false

vim.schedule(function()
  vim.o.clipboard = 'unnamedplus'
end)

vim.o.breakindent = true
vim.o.undofile = true
vim.o.swapfile = false

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.signcolumn = 'yes'
vim.o.updatetime = 250
vim.o.timeoutlen = 300

vim.o.splitright = true
vim.o.splitbelow = true

vim.o.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

vim.o.inccommand = 'split'
vim.o.cursorline = true
vim.o.scrolloff = 0
vim.o.confirm = true
vim.o.winborder = 'rounded'
vim.o.pumborder = 'rounded'

vim.opt.diffopt:remove 'inline:char'
vim.opt.diffopt:append {
  'algorithm:histogram',
  'inline:word',
}
