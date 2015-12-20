#!/bin/bash

sudo apt-get install nodejs npm -y

# make a link so the command is just node
#sudo dpkg-divert --local --divert /usr/bin/node --rename --add /usr/bin/nodejs
sudo ln -sf /usr/bin/nodejs /usr/bin/node

# install some utilities
sudo npm install --global jshint
sudo npm install --global jsxhint
