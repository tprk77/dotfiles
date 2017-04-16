#!/bin/bash

# Add the key for the repo
wget -q -O - "https://dl-ssl.google.com/linux/linux_signing_key.pub" | sudo apt-key add -

# Add the repo (this file might be overwritten)
echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" | \
    sudo tee /etc/apt/sources.list.d/google-chrome.list > /dev/null

# Install Chrome
sudo apt-get update
sudo apt-get install -y --install-suggests google-chrome-stable
