#!/bin/bash

sudo sed -i -e 's;^XKBOPTIONS=""$;XKBOPTIONS="ctrl:nocaps";' /etc/default/keyboard
sudo dpkg-reconfigure keyboard-configuration
