# Needed beacuse of homebrew
fpath+=("/usr/local/share/zsh/site-functions")
# load our own completion functions
fpath=(~/.zsh/completion $fpath)

# completion
autoload -U compinit
compinit

# makes color constants available
autoload -U colors
colors

# enable colored output from ls, etc
export CLICOLOR=1

# history settings
setopt hist_ignore_all_dups inc_append_history
HISTFILE=~/.zhistory
HISTSIZE=4096
SAVEHIST=4096

# Prompts
autoload -U promptinit
promptinit
prompt redhat

# Enable extended globbing
setopt extendedglob

# Allow [ or ] whereever you want
unsetopt nomatch

# handy keybindings
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^R" history-incremental-search-backward
bindkey "^P" history-search-backward
bindkey "^N" insert-last-word

## Aliases
alias ln='ln -v'
alias mkdir='mkdir -p'
alias ..='cd ..'
alias ...='../..'
alias l='ls'
alias ll='ls -al'

# Tmux
alias ta="tmux attach-session -t"
alias tn="tmux new-session -s"

# Git
alias gst="git status"
alias g="git"

# Bundler
alias b="bundle"
alias be='bundle exec'
alias bers='bundle exec rails server'
alias berg='bundle exec rails g'
alias berd='bundle exec rails destroy'

# Editors
alias e='/Applications/Emacs.app/Contents/MacOS/Emacs "$@"'
alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c'
alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"

# OF
alias iof="vim ~/Dropbox/todo.md"

# Servers
alias startpsql="pg_ctl -D /usr/local/var/postgres9.4 -l /usr/local/var/postgres/server.log start"
alias stoppsql="pg_ctl -D /usr/local/var/postgres9.4 stop -s -m fast"

# Ctags
alias gtags="ctags -R *"

# Brew
function brew_search ()
{
  echo -e "***** Brew *****\n $(brew search $1)\n ***** Brew Cask *****\n $(brew cask search $1)"
}

alias bs=brew_search
alias bu="brew update && brew upgrade --all && brew cleanup"

# SSH

# Z
source ~/.zsh/z/z.sh

# Path exports
export PATH="$HOME/.bin:$PATH"

# Haskell/Cabal/Stack
export PATH="~/.cabal/bin/:$PATH"
export PATH="$HOME/Library/Haskell/bin:$PATH"
export PATH="$HOME/Documents/haskell/stack:$PATH"

# recommended by brew doctor
export PATH="/usr/local/bin:$PATH"

# Rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - zsh --no-rehash)"

# Racket
export PATH="/Applications/Racket v6.1.1/bin:$PATH"
