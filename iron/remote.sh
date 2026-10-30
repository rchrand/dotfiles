#!/usr/bin/env bash
# Runs on the Iron machine as dev, started by iron/sync with the payload dir.
# Safe to rerun: links are only created where nothing else lives, and the Mac's
# preferences are merged over machine settings rather than replacing them.
set -euo pipefail
exec </dev/null

payload=${1:?payload directory}
dotfiles="$HOME/dotfiles"
export GIT_TERMINAL_PROMPT=0

step() { printf '\n==> %s\n' "$*"; }

clone_missing() {
  local url="$1" destination="$2" temporary
  if [[ -e $destination || -L $destination ]]; then
    [[ -d $destination/.git ]] && return 0
    printf 'Refusing to replace non-checkout: %s\n' "$destination" >&2
    return 1
  fi
  mkdir -p "$(dirname "$destination")"
  temporary=$(mktemp -d "$destination.clone.XXXXXX")
  git clone -q --depth 1 "$url" "$temporary" || { rm -rf -- "$temporary"; return 1; }
  mv -- "$temporary" "$destination"
}

link_file() {
  local target="$1" link="$2"
  [[ -L $link && $(readlink "$link") == "$target" ]] && return 0
  if [[ -e $link || -L $link ]]; then
    printf 'Refusing to replace existing file: %s\n' "$link" >&2
    return 1
  fi
  mkdir -p "$(dirname "$link")"
  ln -s -- "$target" "$link"
}

# Merge the Mac's JSON preferences over the machine's file (Mac keys win).
merge_json() {
  local source="$1" destination="$2" merged
  [[ -f $source ]] || return 0
  mkdir -p "$(dirname "$destination")"
  merged=$(mktemp "$destination.XXXXXX")
  if [[ -s $destination ]]; then
    jq -s '.[0] * .[1]' "$destination" "$source" >"$merged"
  else
    cp "$source" "$merged"
  fi
  chmod 0600 "$merged"
  mv -- "$merged" "$destination"
}

install_payload() {
  [[ -f $payload/$1 ]] || return 0
  install -D -m "${3:-0644}" -- "$payload/$1" "$2"
}

step "Packages (user Nix profile)"
packages=(bat claude-code codex difftastic eza gcc gnumake nodejs python3 stow tig tree tree-sitter watch zsh)
installed=$(nix profile list --json 2>/dev/null | jq -r '.elements | keys[]' || true)
missing=()
for package in "${packages[@]}"; do
  grep -qx "$package" <<<"$installed" || missing+=("nixpkgs#$package")
done
if ((${#missing[@]})); then
  NIXPKGS_ALLOW_UNFREE=1 nix profile add --impure "${missing[@]}"
fi
if [[ ${IRON_SYNC_UPGRADE:-0} == 1 ]]; then
  NIXPKGS_ALLOW_UNFREE=1 nix profile upgrade --impure --all
fi
export PATH="$HOME/.nix-profile/bin:$PATH"

step "Dotfiles"
clone_missing https://github.com/rchrand/dotfiles.git "$dotfiles"
if [[ -z $(git -C "$dotfiles" status --porcelain) ]]; then
  git -C "$dotfiles" pull -q --ff-only || printf 'warning: could not fast-forward ~/dotfiles\n' >&2
else
  printf 'warning: ~/dotfiles has local changes; not pulling\n' >&2
fi
stow --dir "$dotfiles" --target "$HOME" --restow nvim zsh
link_file "$dotfiles/.tmux.conf" "$HOME/.tmux.conf"
link_file "$dotfiles/.ripgreprc" "$HOME/.ripgreprc"
link_file "$dotfiles/.tigrc" "$HOME/.tigrc"
clone_missing https://github.com/ohmyzsh/ohmyzsh.git "$HOME/.oh-my-zsh"
clone_missing https://github.com/tmux-plugins/tpm.git "$HOME/.tmux/plugins/tpm"

# Machine-local zsh additions; the dotfiles .zshrc sources this file last.
if [[ ! -e $HOME/.zshrc.local ]]; then
  cat >"$HOME/.zshrc.local" <<'ZSH'
# Iron machine-local settings (created by dotfiles iron/sync; edit freely).
path_prepend "$HOME/pax/bin"
alias supermouse="$HOME/pax/bin/pax supervised"
[[ -r "$HOME/.local/state/iron/trust.sh" ]] && source "$HOME/.local/state/iron/trust.sh"
ZSH
fi

# Iron owns ~/.bashrc and the login shell; hand interactive bash over to zsh.
if ! grep -q 'dotfiles iron/sync: zsh' "$HOME/.bashrc" 2>/dev/null; then
  cat >>"$HOME/.bashrc" <<'BASH'

# dotfiles iron/sync: zsh for interactive shells (including tmux panes).
if [[ $- == *i* && -z ${ZSH_VERSION:-} ]] && command -v zsh >/dev/null; then
  SHELL=$(command -v zsh) exec zsh -l
fi
BASH
fi

step "Git preferences"
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
git config --global alias.co checkout
git config --global alias.st status
git config --global alias.pushf "push --force-with-lease"
git config --global pull.rebase true
git config --global fetch.prune true
git config --global init.defaultBranch main
git config --global core.editor nvim
git config --global diff.external difft
git config --global diff.tool difftastic
git config --global difftool.prompt false
# shellcheck disable=SC2016 # git expands these
git config --global difftool.difftastic.cmd 'difft "$LOCAL" "$REMOTE"'
git config --global pager.difftool true

step "Pax and agent preferences"
merge_json "$payload/pax-settings.json" "$HOME/.pax/config/settings.json"
install_payload pax-USER.md "$HOME/.pax/config/USER.md" 0600
merge_json "$payload/claude-settings.json" "$HOME/.claude/settings.json"
install_payload codex-config.toml "$HOME/.codex/config.toml" 0600
install_payload codex-AGENTS.md "$HOME/.codex/AGENTS.md"
install_payload nvim-formatting.lua "$HOME/.config/rchrand/nvim/formatting.lua"

step "tmux plugins"
# TPM reads ~/.tmux.conf; use a private server so live sessions are untouched.
scratch=$(mktemp -d)
cleanup() {
  tmux -S "$scratch/tmux" kill-server 2>/dev/null || true
  rm -rf -- "$scratch"
}
trap cleanup EXIT
tmux -S "$scratch/tmux" -f /dev/null new-session -d -s sync 'sleep 900'
TMUX="$scratch/tmux,0,0" bash -c '
  tmux set-environment -g TMUX_PLUGIN_MANAGER_PATH "$HOME/.tmux/plugins/"
  "$HOME/.tmux/plugins/tpm/bin/install_plugins"
  "$HOME/.tmux/plugins/tpm/bin/update_plugins" all >/dev/null'

step "Neovim plugins, Mason tools and Tree-sitter parsers"
nvim --headless '+qa'
nvim --headless '+MasonToolsInstallSync' '+qa'
cat >"$scratch/parsers.lua" <<'LUA'
local function ready()
  local have = {}
  for _, parser in ipairs(require('nvim-treesitter').get_installed()) do
    have[parser] = true
  end
  for _, parser in ipairs(vim.g.rchrand_treesitter_parsers or {}) do
    if not have[parser] then
      return false
    end
  end
  return true
end
vim.wait(300000, ready, 1000)
LUA
nvim --headless "+luafile $scratch/parsers.lua" '+qa'

# Reload a running tmux server so config changes show up immediately.
tmux source-file "$HOME/.tmux.conf" 2>/dev/null || true

printf '\nDone. One-time sign-ins on the machine: claude (/login), codex login --device-auth, Pax /mcp.\n'
