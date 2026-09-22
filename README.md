# dotfiles

Personal macOS configuration: Emacs, Neovim, tmux, zsh, Git, Ghostty, Helix,
Nix, and command-line tools.

## Install shared editor and shell settings

Install GNU Stow, then link the three portable packages into the home directory:

```sh
stow -t ~ emacs nvim zsh
```

`emacs` provides `~/.emacs.d`; `nvim` provides `~/.config/nvim`; and `zsh`
provides `~/.zshrc` and `~/.aliases`.

Keep credentials and machine-only settings outside Git:

- `~/.zshrc.local` — credentials and work-only shell variables
- `~/.aliases.local` — work-only aliases
- `~/.config/rchrand/emacs/local.el` — work-only Emacs layer
- `~/.config/rchrand/nvim/formatting.lua` — work-only Neovim formatter override

Use `.zshrc.local.example` as the safe starting point.
