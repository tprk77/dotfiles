#!/bin/bash

# Gets the newest Git (necessary for Emacs's Magit)
sudo apt-add-repository ppa:git-core/ppa

# Gets the latest Emacs (necessary for Emacs's Magit)
sudo add-apt-repository ppa:ubuntu-elisp/ppa

# Update and upgrade
sudo apt-get update
sudo apt-get upgrade -y
