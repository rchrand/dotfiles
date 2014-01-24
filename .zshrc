# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# time that oh-my-zsh is loaded.
ZSH_THEME="blinks"

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
alias ls="ls -lF --color"
alias la="ls -laFh"
alias g="git"
alias tmux="tmux -2"
alias ..="cd .."
alias ytmp3="youtube-dl -x --audio-format mp3"
alias syu="packer -Syu"
alias p="packer"
alias mpds="mpd ~/.mpd/mpd.conf"
alias pyserver="python -m http.server"
alias startup="bash ~/scripts/startup.sh"
alias startupdk="bash ~/scripts/startupdk.sh"
alias startred="redshift -l 56.162939:10.203921 -t 5700:3600 -g 0.8 -m vidmode &"
alias be='bundle exec'
alias bers='bundle exec rails server'
alias berg='bundle exec rails g'
alias berd='bundle exec rails destroy'
alias sss='sudo systemctl start'
alias ssr='sudo systemctl restart'
alias s='startup'
alias samd='s && sudo amdcccle'
alias sdk='startupdk'
alias ack="echo 'Use ag!'"
alias qdt="pacman -Qdt"
alias lighttable="./Software/LightTable/LightTable"
alias st="subl3"
alias gogui="bash ~/Games/gogui-1.4.9/bin/gogui"
alias redwm='cd ~/dwm; updpkgsums; makepkg -efi --noconfirm; killall dwm'
alias musickb="echo 'Volume up: Meta-Shift u\nVolume down: Meta-Shift d\nMute alsa: Meta-Shift m\nNext song: Meta-Ctrl j\nPrev song: Meta-Ctrl k\nToggle play: Meta-Ctrl t'"
alias donotturnoff="xset -dpms; xset s off"
alias turnoff="xset dpms force off"

plugins=(git vagrant rails tmux archlinux Tim)

source $ZSH/oh-my-zsh.sh
source ~/git_projects/z/z.sh

# Customize to your needs...
export PATH=$PATH:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/home/rune/.gem/ruby/2.0.0/bin:/home/rune/.rbenv/bin:/home/rune/.cask/bin:/home/rune/.local/bin:

eval "$(rbenv init -)"

PATH="/usr/local/heroku/bin:$PATH"
