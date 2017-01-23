#!/bin/bash

# Add key and repo
wget -q -O - "https://dl-ssl.google.com/linux/linux_signing_key.pub" | sudo apt-key add -
sudo add-apt-repository 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main'

# Install Chrome
sudo apt-get update
sudo apt-get install -y --install-suggests google-chrome-stable
