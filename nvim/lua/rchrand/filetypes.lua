vim.filetype.add {
  extension = {
    rbi = 'ruby',
    sql = 'sql',
  },
  pattern = {
    ['.*%.sql%.j2'] = 'sql',
    ['.*%.sql%.jinja'] = 'sql',
    ['.*%.sql%.jinja2'] = 'sql',
  },
}
