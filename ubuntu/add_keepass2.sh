#!/bin/bash

# Add PPA if we need it
ppa_list=$(grep -RoPish '(?<=ppa.launchpad.net/)[^/]+/[^/ ]+' /etc/apt | sort -u | sed 's/^/ppa:/')
if ! grep "ppa:jtaylor/keepass" <<< "$ppa_list"; then
    sudo add-apt-repository ppa:jtaylor/keepass
    sudo apt-get update
fi

# Install Keepass2 (mono-complete is technically for the Keefox plugin)
sudo apt-get install -y keepass2 mono-complete


# TODO NEW INSTRUCTIONS!
# https://forum.kee.pm/t/installing-kee-instructions/23

# Install Keefox plugin, if it's there
keefox_plgx=$(find ~/.mozilla/ -name "KeePassRPC.plgx" | head -n 1)
if [ -n "$keefox_plgx" ]; then
    # Remove capitalized directory if it exists
    if [ -d /usr/lib/keepass2/Plugins ]; then
        sudo rmdir /usr/lib/keepass2/Plugins
    fi
    # Install plugins
    sudo mkdir -p /usr/lib/keepass2/plugins
    sudo cp "$keefox_plgx" /usr/lib/keepass2/plugins/KeePassRPC.plgx
    echo "Installed Keefox plugin, please restart Firefox"
else
    echo "Could not find Keefox plugin"
fi
