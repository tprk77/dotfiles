#!/bin/bash

# Installs some common packages with `apt-get`. Assumes that
# the software sources have been setup appropriately.

general_utils="lftp tree curl jq sshfs"
dev_tools="silversearcher-ag dos2unix"
build_tools="cmake autoconf automake libtool"
zip_tools="p7zip-full p7zip-rar"
fs_utils="exfat-fuse exfat-utils"
emacs_utils="libclang-6.0-dev"

sudo apt-get install -y \
     $general_utils \
     $dev_tools \
     $build_tools \
     $zip_tools \
     $fs_utils \
     $emacs_utils
