# rchrand nvim

Bare Neovim config for 0.12+ using builtin `vim.pack`.

## Plugins

Plugin setup lives in `lua/rchrand/plugins/`.

- `:PackUpdate` wraps `vim.pack.update()` with completion; `:PackUpdate!` skips confirmation.
- `:PackSync` syncs installed plugins from `nvim-pack-lock.json`; `:PackSync!` skips confirmation.
- `:packdel <name>` removes inactive plugins after removing their `vim.pack.add()` spec.

## Notes

- `tree-sitter` CLI is optional. If installed, configured parsers are installed on startup.
- `make` enables the `telescope-fzf-native` build.
- `fff.nvim` is the default file/content picker on `<leader>sf`, `<leader>sg`, `<leader>sw`, `<leader>sn`, and `<C-p>`.
- Python uses `ty` for language intelligence and Ruff for linting, imports, and formatting. `:TyToggle` and `:RuffToggle` toggle them for current and future buffers.
- `direnv.vim` loads per-project Nix/dev-shell environments before LSPs resolve tools.
- Ruby LSP is launched outside the project bundle from direnv, asdf, or `PATH`; Sorbet is additionally enabled when the project bundle provides it. C/C++ uses `clangd` with `<leader>ch` for source/header switching.
- Neovim's default LSP mappings are used, with `gd`, `gD`, and `gW` added for definition, declaration, and workspace-symbol navigation.
- `:RaylibCompileFlags` writes `compile_flags.txt` from `pkg-config --cflags raylib` for small raylib projects.
