## Load zplug
# [[ -r "${HOME}/.zplug/init.zsh" ]] || git clone https://github.com/zplug/zplug.git "${HOME}/.zplug"
# source "${HOME}/.zplug/init.zsh"

# ## speed up zplug. See https://github.com/zplug/zplug/issues/368#issuecomment-282566102
# __zplug::io::file::generate

# # let zplug manage itself
# zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# zplug "plugins/git", from:oh-my-zsh
# # zplug "plugins/rails", from:oh-my-zsh
# zplug "plugins/z", from:oh-my-zsh
# zplug "plugins/osx", from:oh-my-zsh
# zplug "plugins/helm", from:oh-my-zsh
# zplug "plugins/asdf", from:oh-my-zsh
# zplug "plugins/kubectl", from:oh-my-zsh
# zplug "mafredri/zsh-async", from:github
# zplug "superbrothers/zsh-kubectl-prompt", from:github
# zplug "sindresorhus/pure", from:github, use:pure.zsh, as:theme

# ## Install if not installed
# zplug check || zplug install

# ## Then, source plugins and add commands to $PATH
# zplug load
#### --------------------------------------------------------------------------

## Antigen
#
source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle z
antigen bundle helm
antigen bundle kubectl
antigen bundle asdf
antigen bundle zsh-kubectl-prompt

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme refined

# Tell Antigen that you're done.
antigen apply

# Aliases
source ~/.aliases

# Add `~/bin` to the `$PATH`
# export PATH="$HOME/bin:$PATH";
export PATH="/usr/local/sbin:$PATH";
# export PATH="$HOME/.local/bin:$PATH"
# export PATH="/usr/local/bin:$PATH";


# Rust
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="/Users/rchrand/rust/rustc-1.14.0/src"

# Postgres
export PATH="/Applications/Postgres.app/Contents/Versions/10/bin:$PATH";
export PATH="/Users/rchrand/anaconda3/bin:$PATH"

# added by Miniconda3 installer
export PATH="/Users/rchrand/miniconda3/bin:$PATH"
export PATH=/Users/rchrand/.local/bin:$PATH

# Google Cloud
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'

# Standard UTF8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

bindkey -e

## Command history configuration
if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi
#
    HISTSIZE=100000
    SAVEHIST=100000

#    # Show history
case $HIST_STAMPS in
        "mm/dd/yyyy") alias history='fc -fl 1' ;;
        "dd.mm.yyyy") alias history='fc -El 1' ;;
        "yyyy-mm-dd") alias history='fc -il 1' ;;
        *) alias history='fc -l 1' ;;
esac

# Prompt
RPROMPT='%{$fg[blue]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}'

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
setopt hist_ignore_all_dups

# NVM
export NVM_DIR="$HOME/.nvm"
source "/usr/local/opt/nvm/nvm.sh"

# ASDF
source $HOME/.asdf/asdf.sh
source $HOME/.asdf/completions/asdf.bash


# heroku autocomplete setup
HEROKU_AC_ZSH_SETUP_PATH=/Users/rchrand/Library/Caches/heroku/autocomplete/zsh_setup && test -f $HEROKU_AC_ZSH_SETUP_PATH && source $HEROKU_AC_ZSH_SETUP_PATH;


# export EXA_COLORS="uu=38;5;249:un=38;5;241:gu=38;5;245:gn=38;5;241:da=38;5;245:sn=38;5;7:sb=38;5;7:ur=38;5;3;1:uw=38;5;5;1:ux=38;5;1;1:ue=38;5;1;1:gr=38;5;249:gw=38;5;249:gx=38;5;249:tr=38;5;249:tw=38;5;249:tx=38;5;249:fi=38;5;248:di=38;5;253:ex=38;5;1:xa=38;5;10:*.png=38;5;4:*.jpg=38;5;4:*.gif=38;5;4"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
