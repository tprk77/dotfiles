#!/bin/bash

general_utils="lftp tree curl sshfs"
dev_tools="silversearcher-ag dos2unix"
build_tools="cmake autoconf automake libtool"

# add some of my favorite utilities
sudo apt-get install -y \
     $general_utils \
     $dev_tools \
     $build_tools
