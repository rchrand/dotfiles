local function apply_flexoki_contrast()
  if vim.g.colors_name ~= 'flexoki' then
    return
  end

  if vim.o.background == 'light' then
    local bg = '#FFFCF0'
    local fg = '#100F0F'
    local muted = '#6F6E69'
    local add_bg = '#EBF2E7'
    local del_bg = '#F7D7D4'
    local change_bg = '#FCEEB8'

    vim.api.nvim_set_hl(0, 'Normal', { bg = bg })
    vim.api.nvim_set_hl(0, 'NormalFloat', { bg = bg })
    vim.api.nvim_set_hl(0, 'SignColumn', { bg = bg })
    vim.api.nvim_set_hl(0, 'EndOfBuffer', { bg = bg })
    vim.api.nvim_set_hl(0, 'Comment', { fg = muted })
    vim.api.nvim_set_hl(0, '@comment', { fg = muted })
    vim.api.nvim_set_hl(0, 'LineNr', { fg = muted })
    vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = fg, bold = true })

    vim.api.nvim_set_hl(0, 'Identifier', { fg = '#205EA6' })
    vim.api.nvim_set_hl(0, 'Function', { fg = '#BC5215', bold = true })
    vim.api.nvim_set_hl(0, 'Keyword', { fg = '#66800B', bold = true })
    vim.api.nvim_set_hl(0, 'Type', { fg = '#66800B', bold = true })
    vim.api.nvim_set_hl(0, 'String', { fg = '#24837B' })

    -- Neogit diff visibility (light)
    vim.api.nvim_set_hl(0, 'NeogitDiffAdd', { fg = '#66800B', bg = add_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffAddHighlight', { fg = '#66800B', bg = add_bg, bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffDelete', { fg = '#AF3029', bg = del_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffDeleteHighlight', { fg = '#AF3029', bg = del_bg, bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffContext', { fg = muted, bg = bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffContextHighlight', { fg = fg, bg = change_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffHeader', { fg = fg, bg = '#E6E4D9', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffHeaderHighlight', { fg = fg, bg = '#DAD8CE', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitHunkHeader', { fg = fg, bg = '#E6E4D9', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitHunkHeaderHighlight', { fg = fg, bg = '#DAD8CE', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffAdditions', { fg = '#66800B', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffDeletions', { fg = '#AF3029', bold = true })
  else
    local bg = '#100F0F'
    local fg = '#CECDC3'
    local muted = '#878580'
    local add_bg = '#142625'
    local del_bg = '#2D1F1E'
    local change_bg = '#4D3A0B'

    vim.api.nvim_set_hl(0, 'Normal', { bg = bg })
    vim.api.nvim_set_hl(0, 'NormalFloat', { bg = bg })
    vim.api.nvim_set_hl(0, 'SignColumn', { bg = bg })
    vim.api.nvim_set_hl(0, 'EndOfBuffer', { bg = bg })
    vim.api.nvim_set_hl(0, 'Comment', { fg = muted })
    vim.api.nvim_set_hl(0, '@comment', { fg = muted })
    vim.api.nvim_set_hl(0, 'LineNr', { fg = muted })
    vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = fg, bold = true })

    -- Neogit diff visibility (dark)
    vim.api.nvim_set_hl(0, 'NeogitDiffAdd', { fg = '#879A39', bg = add_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffAddHighlight', { fg = '#879A39', bg = add_bg, bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffDelete', { fg = '#D14D41', bg = del_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffDeleteHighlight', { fg = '#D14D41', bg = del_bg, bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffContext', { fg = muted, bg = bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffContextHighlight', { fg = fg, bg = change_bg })
    vim.api.nvim_set_hl(0, 'NeogitDiffHeader', { fg = fg, bg = '#343331', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffHeaderHighlight', { fg = fg, bg = '#403E3C', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitHunkHeader', { fg = fg, bg = '#343331', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitHunkHeaderHighlight', { fg = fg, bg = '#403E3C', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffAdditions', { fg = '#879A39', bold = true })
    vim.api.nvim_set_hl(0, 'NeogitDiffDeletions', { fg = '#D14D41', bold = true })
  end
end

vim.api.nvim_create_autocmd('ColorScheme', {
  callback = apply_flexoki_contrast,
})

local function set_theme(theme)
  vim.g.rchrand_theme = theme
  if theme == 'flexoki-light' then
    vim.o.background = 'light'
    vim.cmd.colorscheme 'flexoki-light'
  elseif theme == 'material-darker' then
    vim.o.background = 'dark'
    vim.g.material_style = 'darker'
    vim.cmd.colorscheme 'material'
  else
    vim.o.background = 'dark'
    vim.cmd.colorscheme 'flexoki-dark'
  end

  apply_flexoki_contrast()
end

local function system_is_dark()
  if vim.fn.has 'mac' == 1 then
    local result
    if vim.system then
      result = vim.system({ 'defaults', 'read', '-g', 'AppleInterfaceStyle' }, { text = true }):wait()
      if result.code == 0 and result.stdout:match 'Dark' then
        return true
      end
    else
      local output = vim.fn.system { 'defaults', 'read', '-g', 'AppleInterfaceStyle' }
      if vim.v.shell_error == 0 and output:match 'Dark' then
        return true
      end
    end
    return false
  end

  return vim.o.background == 'dark'
end

local function apply_system_theme()
  if vim.g.theme_auto == false then
    return
  end

  local theme = system_is_dark() and 'material-darker' or 'flexoki-light'
  if vim.g.rchrand_theme ~= theme then
    set_theme(theme)
  end
end

vim.api.nvim_create_user_command('ThemeLight', function()
  vim.g.theme_auto = false
  set_theme 'flexoki-light'
end, {})

vim.api.nvim_create_user_command('ThemeDark', function()
  vim.g.theme_auto = false
  set_theme 'material-darker'
end, {})

vim.api.nvim_create_user_command('ThemeToggle', function()
  vim.g.theme_auto = false
  if vim.g.rchrand_theme == 'flexoki-light' then
    set_theme 'material-darker'
  else
    set_theme 'flexoki-light'
  end
end, {})

vim.api.nvim_create_user_command('ThemeAuto', function()
  vim.g.theme_auto = true
  apply_system_theme()
end, {})

vim.api.nvim_create_user_command('ThemeRefresh', function()
  if vim.g.theme_auto ~= false then
    local theme = system_is_dark() and 'material-darker' or 'flexoki-light'
    set_theme(theme)
  else
    set_theme(vim.g.rchrand_theme)
  end
end, {})

vim.api.nvim_create_autocmd({ 'VimEnter', 'FocusGained' }, {
  callback = apply_system_theme,
})

return {
  apply_flexoki_contrast = apply_flexoki_contrast,
  set_theme = set_theme,
  apply_system_theme = apply_system_theme,
}
