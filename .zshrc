# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# time that oh-my-zsh is loaded.
ZSH_THEME="mh"

export BROWSER="firefox"
export EDITOR="vim"
export LC_ALL="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
[ -n "$TMUX" ] && export TERM=screen-256color

[[ $TTY == "/dev/tty1" ]] && exec startx

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
alias ls="ls --color -F"
alias la="ls -laFh"
alias g="git"
alias apti="sudo apt-get install"
alias aptu="sudo apt-get update; sudo apt-get dist-upgrade"
alias tmux="tmux -2"
alias ..="cd .."
alias ytmp3="youtube-dl -x --audio-format mp3"
alias syu="packer -Syu"
alias p="packer"
alias mpds="mpd ~/.mpd/mpd.conf"



# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vagrant vi-mode)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
