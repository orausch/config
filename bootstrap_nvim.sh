#!/usr/bin/env bash
set -eux -o pipefail


if [ ! -d $HOME/.local/share/nvim-linux64/ ]; then
  wget https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.tar.gz
  mkdir -p ~/.local/share
  tar -xzf nvim-linux64.tar.gz --directory ~/.local/share
  mkdir -p ~/.local/bin
  ln -s -t ~/.local/bin ~/.local/share/nvim-linux64/bin/nvim
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
fi


# USAGE: simple_install $URL $SIMPLE_NAME $BINARY_PATH
simple_install() {
  if [ ! -d "$HOME/.local/share/$2/" ]; then
    wget "$1" -O "$2.tar.gz"
    mkdir -p "$HOME/.local/share/$2"
    tar -xzf "$2.tar.gz" --directory "$HOME/.local/share/$2" --strip-components 1
    mkdir -p $HOME/.local/bin
    ln -s -t $HOME/.local/bin "$HOME/.local/share/$2/$3"
  fi
}
FD_URL=https://github.com/sharkdp/fd/releases/download/v8.4.0/fd-v8.4.0-x86_64-unknown-linux-gnu.tar.gz
RG_URL=https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep-13.0.0-x86_64-unknown-linux-musl.tar.gz
simple_install $FD_URL fd fd
simple_install $RG_URL rg rg

