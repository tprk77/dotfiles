#!/bin/bash

# Add key and repo
wget -q -O - "http://deb.opera.com/archive.key" | sudo apt-key add -
sudo add-apt-repository 'deb http://deb.opera.com/opera-stable/ stable non-free'

# Update and install, this will also install some TTF fonts
sudo apt-get update
sudo apt-get install -y --install-suggests opera-stable pepperflashplugin-nonfree

# NOTE: Opera doesn't currently use Pepper Flash Player, but it might soon
