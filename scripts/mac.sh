#!/usr/bin/env bash

# Welcome to the thoughtbot laptop script!
# Be prepared to turn your laptop (or desktop, no haters here)
# into an awesome development machine.

fancy_echo() {
  printf "\n%b\n" "$1"
}

append_to_zshrc() {
  local text="$1" zshrc
  local skip_new_line="$2"

  if [[ -w "$HOME/.zshrc.local" ]]; then
    zshrc="$HOME/.zshrc.local"
  else
    zshrc="$HOME/.zshrc"
  fi

  if ! grep -Fqs "$text" "$zshrc"; then
    if (( skip_new_line )); then
      printf "%s\n" "$text" >> "$zshrc"
    else
      printf "\n%s\n" "$text" >> "$zshrc"
    fi
  fi
}

trap 'ret=$?; test $ret -ne 0 && printf "failed\n\n" >&2; exit $ret' EXIT

set -e

if [[ ! -d "$HOME/.bin/" ]]; then
  mkdir "$HOME/.bin"
fi

if [ ! -f "$HOME/.zshrc" ]; then
  touch "$HOME/.zshrc"
fi

append_to_zshrc 'export PATH="$HOME/.bin:$PATH"'

fancy_echo "Changing your shell to zsh ..."
  chsh -s $(which zsh)

brew_install_or_upgrade() {
  if brew_is_installed "$1"; then
    if brew_is_upgradable "$1"; then
      brew upgrade "$@"
    fi
  else
    brew install "$@"
  fi
}

brew_is_installed() {
  local NAME=$(brew_expand_alias "$1")

  brew list -1 | grep -Fqx "$NAME"
}

brew_is_upgradable() {
  local NAME=$(brew_expand_alias "$1")

  brew outdated --quiet "$NAME" >/dev/null
  [[ $? -ne 0 ]]
}

brew_expand_alias() {
  brew info "$1" 2>/dev/null | head -1 | awk '{gsub(/:/, ""); print $1}'
}

brew_launchctl_restart() {
  local NAME=$(brew_expand_alias "$1")
  local DOMAIN="homebrew.mxcl.$NAME"
  local PLIST="$DOMAIN.plist"

  mkdir -p ~/Library/LaunchAgents
  ln -sfv /usr/local/opt/$NAME/$PLIST ~/Library/LaunchAgents

  if launchctl list | grep -q $DOMAIN; then
    launchctl unload ~/Library/LaunchAgents/$PLIST >/dev/null
  fi
  launchctl load ~/Library/LaunchAgents/$PLIST >/dev/null
}

if [[ -f /etc/zshenv ]]; then
  fancy_echo "Fixing OSX zsh environment bug ..."
    sudo mv /etc/{zshenv,zshrc}
fi

if ! command -v brew >/dev/null; then
  fancy_echo "Installing Homebrew, a good OS X package manager ..."
    ruby <(curl -fsS https://raw.githubusercontent.com/Homebrew/install/master/install)

    append_to_zshrc '# recommended by brew doctor'
    append_to_zshrc 'export PATH="/usr/local/bin:$PATH"' 1
    export PATH="/usr/local/bin:$PATH"
else
  fancy_echo "Homebrew already installed. Skipping ..."
fi

fancy_echo "Updating Homebrew formulas ..."
brew update

fancy_echo "Installing Postgres, a good open source relational database ..."
  brew_install_or_upgrade 'postgres'

fancy_echo "Installing The Silver Searcher (better than ack or grep) to search the contents of files ..."
  brew_install_or_upgrade 'the_silver_searcher'

fancy_echo "Installing vim from Homebrew to get the latest version ..."
  brew_install_or_upgrade 'vim --override-system-vi'

fancy_echo "Installing ctags, to index files for vim tab completion of methods, classes, variables ..."
  brew_install_or_upgrade 'ctags'

fancy_echo "Installing tmux, to save project state and switch between projects ..."
  brew_install_or_upgrade 'tmux'

fancy_echo "Installing reattach-to-user-namespace, for copy-paste and RubyMotion compatibility with tmux ..."
  brew_install_or_upgrade 'reattach-to-user-namespace'

fancy_echo "Installing ImageMagick, to crop and resize images ..."
  brew_install_or_upgrade 'imagemagick'

node_version="0.10"

fancy_echo "Installing NVM, Node.js, and NPM, for running apps and installing JavaScript packages ..."
  brew_install_or_upgrade 'nvm'

  append_to_zshrc 'export PATH="$PATH:/usr/local/lib/node_modules"'
  append_to_zshrc 'source $(brew --prefix nvm)/nvm.sh' 1

  source $(brew --prefix nvm)/nvm.sh
  nvm install "$node_version"

  fancy_echo "Setting $node_version as the global default nodejs..."
  nvm alias default "$node_version"

fancy_echo "Starting Postgres ..."
  brew_launchctl_restart postgresql

if [[ ! -d "$HOME/.rbenv" ]]; then
  fancy_echo "Installing rbenv, to change Ruby versions ..."
    git clone https://github.com/sstephenson/rbenv.git ~/.rbenv

    append_to_zshrc 'export PATH="$HOME/.rbenv/bin:$PATH"'
    append_to_zshrc 'eval "$(rbenv init - zsh --no-rehash)"' 1

    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init - zsh)"
fi

if [[ ! -d "$HOME/.rbenv/plugins/rbenv-gem-rehash" ]]; then
  fancy_echo "Installing rbenv-gem-rehash so the shell automatically picks up binaries after installing gems with binaries..."
    git clone https://github.com/sstephenson/rbenv-gem-rehash.git \
      ~/.rbenv/plugins/rbenv-gem-rehash
fi

if [[ ! -d "$HOME/.rbenv/plugins/ruby-build" ]]; then
  fancy_echo "Installing ruby-build, to install Rubies ..."
    git clone https://github.com/sstephenson/ruby-build.git \
      ~/.rbenv/plugins/ruby-build
fi

fancy_echo "Upgrading and linking OpenSSL ..."
  brew_install_or_upgrade 'openssl'
  brew unlink openssl && brew link openssl --force

ruby_version="$(curl -sSL http://ruby.thoughtbot.com/latest)"

fancy_echo "Installing Ruby $ruby_version ..."
  rbenv install -s "$ruby_version"

fancy_echo "Setting $ruby_version as global default Ruby ..."
  rbenv global "$ruby_version"
  rbenv rehash

fancy_echo "Updating to latest Rubygems version ..."
  gem update --system

fancy_echo "Installing Bundler to install project-specific Ruby gems ..."
  gem install bundler --no-document --pre

fancy_echo "Configuring Bundler for faster, parallel gem installation ..."
  number_of_cores=$(sysctl -n hw.ncpu)
  bundle config --global jobs $((number_of_cores - 1))

fancy_echo "Installing Heroku CLI client ..."
  brew_install_or_upgrade 'heroku-toolbelt'

fancy_echo "Installing the heroku-config plugin to pull config variables locally to be used as ENV variables ..."
  heroku plugins:install git://github.com/ddollar/heroku-config.git

fancy_echo "************* FINISHED WITH THOUGHTBOT ********************"
fancy_echo "Making the dir and pulling z into it"
  mkdir -p ~/Documents/utilities
  git clone https://github.com/rupa/z.git ~/Documents/utilities/z

fancy_echo "Installing brew cask"
  brew tap caskroom/cask
  brew install brew-cask

fancy_echo "Installing Flux"
  brew cask install flux

fancy_echo "Pulling dotfiles in at ~/dotfiles"
  git clone https://github.com/runeandersen92/dotfiles.git ~/dotfiles

fancy_echo "Running some better defaults for mac"
  defaults write -g ApplePressAndHoldEnabled -bool false

  #Use current directory as default search scope in Finder
  defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

  #Show Path bar in Finder
  defaults write com.apple.finder ShowPathbar -bool true

  #Show Status bar in Finder
  defaults write com.apple.finder ShowStatusBar -bool true

  #Show indicator lights for open applications in the Dock
  defaults write com.apple.dock show-process-indicators -bool true

  #Enable AirDrop over Ethernet and on unsupported Macs running Lion
  defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

  #Set a blazingly fast keyboard repeat rate
  defaults write NSGlobalDomain KeyRepeat -int 0.02

  #Set a shorter Delay until key repeat
  defaults write NSGlobalDomain InitialKeyRepeat -int 12

  #Enable Safari’s debug menu
  defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

  #Add a context menu item for showing the Web Inspector in web views
  defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

  #Show the ~/Library folder
  chflags nohidden ~/Library
fancy_echo "Better defaults done"

fancy_echo "Things that needs to get installed now:"
fancy_echo "1. Bitcasa"
fancy_echo "2. 1Password"
fancy_echo "3. Dash"
fancy_echo "4. Airmail"
fancy_echo "5. Slate"
fancy_echo "6. iTerm 2"
fancy_echo "7. Xcode 6"
