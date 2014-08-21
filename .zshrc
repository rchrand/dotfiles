# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="norm"
export EDITOR="vim"

# Standard
alias ls="ls -lF --color"
alias la="ls -laFh"
alias g="git"

# Linux
alias tmux="TERM=screen-256color tmux -2"
alias ytmp3="youtube-dl -x --audio-format mp3"
alias mpds="mpd ~/.mpd/mpd.conf"

# Python
alias pyserver="python -m http.server"

# Ruby/Rails
alias be='bundle exec'
alias bers='bundle exec rails server'
alias berg='bundle exec rails g'
alias berd='bundle exec rails destroy'

#Ctags
alias ctagsruby='ctags --recuse --languages=ruby --exclude=.git --exclude=log .'

# Editors
alias e="emacsclient -c &"
alias m="mvim"

# Servers
alias startpsql="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias stoppsql="pg_ctl -D /usr/local/var/postgres stop -s -m fast"

alias startmongo="mongod --dbpath /Users/rune/mongodb/data/db"

# Brew
function brew_search ()
{
  echo -e "*** Brew ***\n $(brew search $1)\n *** Brew Cask ***\n $(brew cask search $1)"
}

alias bs=brew_search
alias bu="brew update && brew upgrade"

plugins=(git vagrant rails tmux)

source $ZSH/oh-my-zsh.sh
source ~/Documents/projects/z/z.sh

# Customize to your needs...
export PATH=$PATH:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/home/rune/.gem/ruby/2.0.0/bin:/home/rune/.rbenv/bin:/home/rune/.cask/bin:/home/rune/.local/bin:~/.rbenv/shims:

export PATH=/usr/local/bin:$PATH
# Rbenv
eval "$(rbenv init -)"

# Pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
