#!/usr/bin/env bash
set -eux -o pipefail


wget https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.tar.gz
mkdir -p ~/.local/share
tar -xzf nvim-linux64.tar.gz --directory ~/.local/share
mkdir -p ~/.local/bin
ln -s -t ~/.local/bin ~/.local/share/nvim-linux64/bin/nvim
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
