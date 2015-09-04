#!/bin/bash

# gets the newest Git (necessary for Emacs's Magit)
sudo apt-add-repository ppa:git-core/ppa

# gets the latest Emacs (necessary for Emacs's Magit)
sudo apt-add-repository ppa:alexmurray/emacs

# update and upgrade
sudo apt-get update
sudo apt-get upgrade -y
