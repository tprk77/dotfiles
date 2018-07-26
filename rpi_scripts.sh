#!/bin/bash


## # KEYBOARD CONFIGURATION FILE
##
## # Consult the keyboard(5) manual page.
##
## XKBMODEL="pc104"
## XKBLAYOUT="us"
## XKBVARIANT=""
## XKBOPTIONS="ctrl:nocaps"
##
## BACKSPACE="guess"

# ???
# sudo debconf-show keyboard-configuration

# sudo sed -i -e 's;^XKBMODEL=.*$;XKBMODEL="pc104";' /etc/default/keyboard
# sudo sed -i -e 's;^XKBLAYOUT=.*$;XKBLAYOUT="us";' /etc/default/keyboard
# sudo dpkg-reconfigure -f noninteractive keyboard-configuration


# https://stackoverflow.com/a/20693661

sudo debconf-set-selections <<EOF
tzdata tzdata/Areas select America
tzdata tzdata/Zones/America select New_York
tzdata tzdata/Zones/Etc select UTC
EOF
sudo rm /etc/timezone
sudo rm /etc/localtime
sudo dpkg-reconfigure -f noninteractive tzdata

# Else...
# echo "America/New_York" > /etc/timezone
# dpkg-reconfigure -f noninteractive tzdata


sudo debconf-set-selections <<EOF
locales locales/locales_to_be_generated multiselect en_US.UTF-8 UTF-8
locales locales/default_environment_locale select en_US.UTF-8
EOF
sudo dpkg-reconfigure -f noninteractive locales


sudo debconf-set-selections <<EOF
keyboard-configuration keyboard-configuration/model select Generic 104-key PC
keyboard-configuration keyboard-configuration/layout select English (US)
keyboard-configuration keyboard-configuration/variant select English (US)
keyboard-configuration keyboard-configuration/unsupported_config_options boolean true
keyboard-configuration keyboard-configuration/optionscode string ctrl:nocaps
EOF
sudo dpkg-reconfigure -f noninteractive locales

# Post?
# https://raspberrypi.stackexchange.com/questions/236/simple-keyboard-configuration
## sudo debconf-get-selections | grep keyboard-configuration
