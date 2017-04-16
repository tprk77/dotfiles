#!/bin/bash

# Get the key for the repo
sudo apt-key adv --keyserver "hkp://keyserver.ubuntu.com:80" \
     --recv-keys "BBEBDCB318AD50EC6865090613B00F1FD2C19886"

# Add the repo
echo "deb http://repository.spotify.com stable non-free" | \
    sudo tee /etc/apt/sources.list.d/spotify.list > /dev/null

# Update and install Spotify!
sudo apt-get update
sudo apt-get install -y spotify-client
