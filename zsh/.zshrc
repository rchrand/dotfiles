# Interactive zsh setup
export ZSH="$HOME/.oh-my-zsh"
plugins=(git z fzf)
ZSH_THEME="robbyrussell"

# Keep Homebrew, local tools, and completions predictable across login shells.
fpath=("$HOME/.zfunc" /opt/homebrew/share/zsh/site-functions $fpath)
typeset -U fpath

path_prepend() {
  [[ -d "$1" ]] || return
  path=(${path:#"$1"})
  path=("$1" $path)
}

path_append() {
  [[ -d "$1" ]] || return
  path=(${path:#"$1"})
  path=($path "$1")
}

[[ -r "$ZSH/oh-my-zsh.sh" ]] && source "$ZSH/oh-my-zsh.sh"
[[ -r "$HOME/.aliases" ]] && source "$HOME/.aliases"
[[ -r "$HOME/.aliases.local" ]] && source "$HOME/.aliases.local"
[[ -r "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export EDITOR=nvim
export VISUAL=nvim

bindkey -e
HISTFILE=${HISTFILE:-$HOME/.zsh_history}
HISTSIZE=100000
SAVEHIST=100000
setopt append_history extended_history hist_expire_dups_first hist_ignore_dups
setopt hist_ignore_space hist_verify inc_append_history share_history
setopt hist_ignore_all_dups ignore_eof

case $HIST_STAMPS in
  mm/dd/yyyy) alias history='fc -fl 1' ;;
  dd.mm.yyyy) alias history='fc -El 1' ;;
  yyyy-mm-dd) alias history='fc -il 1' ;;
  *) alias history='fc -l 1' ;;
esac

path_prepend /opt/homebrew/opt/node@22/bin
path_prepend "$HOME/.local/bin"
path_prepend "$HOME/scripts"
path_prepend "$HOME/.cargo/bin"
path_prepend /opt/homebrew/opt/rustup/bin
path_prepend /opt/homebrew/opt/postgresql@17/bin
path_append "$HOME/.lmstudio/bin"

if [[ -d /nix/var/nix/profiles/default/bin ]]; then
  path_prepend /nix/var/nix/profiles/default/bin
  path_prepend "$HOME/.nix-profile/bin"
fi

export LDFLAGS="-L/opt/homebrew/opt/postgresql@17/lib ${LDFLAGS:-}"
export CPPFLAGS="-I/opt/homebrew/opt/postgresql@17/include ${CPPFLAGS:-}"

(( $+commands[direnv] )) && eval "$(direnv hook zsh)"
alias dbtf="$HOME/.local/bin/dbt"
alias pilocal='~/projects/local-models/bin/pi-local --provider lmstudio --model google/gemma-4-12b-qat'
alias pione='~/projects/local-models/bin/pi-one'

# Work credentials and host-specific variables stay local and never enter Git.
[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"
