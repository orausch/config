#!/usr/bin/env bash
set -eux -o pipefail

wget https://github.com/clangd/clangd/releases/download/13.0.0/clangd-linux-13.0.0.zip
mkdir -p ~/.local/share
unzip clangd-linux-13.0.0.zip -d ~/.local/share
mkdir -p ~/.local/bin
ln -s -t ~/.local/bin ~/.local/share/clangd_13.0.0/bin/clangd

