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

## Sync to an Iron machine

```sh
iron/sync [machine] [--upgrade]   # default machine: rust-in-peace
```

Pulls these dotfiles into the machine's `~/dotfiles` (push first), links them,
installs the CLI tools and agents into the user Nix profile, and copies the
portable Pax, Claude and Codex preferences plus untracked machine files
(`USER.md`, Codex `AGENTS.md`, the Neovim formatter). Rerun any time; `--upgrade`
also upgrades the Nix packages. Sign in to Claude, Codex and Pax `/mcp` on the
machine once; credentials are never copied.
