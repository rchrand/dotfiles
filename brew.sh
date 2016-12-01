#!/usr/bin/env bash

# Tap all the resources
brew tap 'caskroom/cask'
brew tap 'homebrew/bundle'
brew tap 'homebrew/core'
brew tap 'homebrew/services'
brew tap 'homebrew/versions'
brew tap 'homebrew/science'
brew tap 'homebrew/completions'

# Make sure we’re using the latest Homebrew.
brew update

# Upgrade any already-installed formulae.
brew upgrade

# Install GNU core utilities (those that come with macOS are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
brew install coreutils

# Install some other useful utilities like `sponge`.
brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names
# Install Bash 4.
# Note: don’t forget to add `/usr/local/bin/bash` to `/etc/shells` before
# running `chsh`.
brew install bash
brew install bash-completion2

# Switch to using brew-installed bash as default shell
if ! fgrep -q '/usr/local/bin/bash' /etc/shells; then
  echo '/usr/local/bin/bash' | sudo tee -a /etc/shells;
  chsh -s /usr/local/bin/bash;
fi;

# Install `wget` with IRI support.
brew install wget --with-iri

# Install more recent versions of some macOS tools.
brew install vim --override-system-vi
brew install homebrew/dupes/grep
brew install homebrew/dupes/openssh
brew install binutils

# Install other useful binaries.
brew install dark-mode
brew install git
brew install git-lfs
brew install imagemagick --with-webp
brew install tree

# My own stuff
brew install irssi
brew install redis
brew install the_silver_searcher
brew install neovim/neovim/neovim
brew install rabbitmq
brew install emacs --with-cocoa --with-gnutls --with-imagemagick --HEAD
brew install heroku
brew install ledger

# Languages
brew install r
brew install elixir
brew install python
brew install mix-completion
brew install rbenv
brew instal leiningen

# Casks
brew cask install 'appcleaner'
brew cask install 'flux'
# brew cask install 'postgres' # Download old version to match heroku
brew cask install 'vlc'
brew cask install 'rstudio'
brew cask install 'macdown'
brew cask install 'teamspeak-client'
brew cask install 'battle-net'
brew cask install 'steam'
brew cask install 'kindle'

# Remove outdated versions from the cellar.
brew cleanup
