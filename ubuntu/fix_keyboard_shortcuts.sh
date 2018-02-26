#!/bin/bash

# Fixes the default keyboard shortcuts to work with Emacs,
# etc. Removes annoying defaults. Maybe adds other shortcuts.

### GNOME ###

# # Zero all shortcuts
# for keybinding in $(gsettings list-keys org.gnome.desktop.wm.keybindings); do
#     dconf write /org/gnome/desktop/wm/keybindings/$keybinding "@as []"
# done

# # Add back shortcuts we want
# dconf write /org/gnome/desktop/wm/keybindings/close "@as ['<Alt>F4']"
# dconf write /org/gnome/desktop/wm/keybindings/panel-run-dialog "@as ['<Alt>F2']"
# dconf write /org/gnome/desktop/wm/keybindings/show-desktop "@as ['<Super>d']"
# dconf write /org/gnome/desktop/wm/keybindings/switch-applications "@as ['<Super>Tab']"
# dconf write /org/gnome/desktop/wm/keybindings/switch-applications-backward "@as ['<Super><Shift>Tab']"
# dconf write /org/gnome/desktop/wm/keybindings/maximize "@as ['<Super>Up']"
# dconf write /org/gnome/desktop/wm/keybindings/unmaximize "@as ['<Super>Down']"
# dconf write /org/gnome/desktop/wm/keybindings/minimize "@as ['<Super>KP_0']"

### MATE ###

# TODO Also process /org/mate/macro/general, etc
# gsettings list-keys org.mate.Marco.global-keybindings
# gsettings set org.mate.Marco.global-keybindings cycle-group 'disabled'
# gsettings set org.mate.Marco.global-keybindings cycle-group '<Alt>F7'

# Zero all shortcuts
for keybinding in $(gsettings list-keys org.mate.Marco.global-keybindings); do
    gsettings set org.mate.Marco.global-keybindings $keybinding "disabled"
done
for keybinding in $(gsettings list-keys org.mate.Marco.window-keybindings); do
    gsettings set org.mate.Marco.window-keybindings $keybinding "disabled"
done

# Add back shortcuts we want
gsettings set org.mate.Marco.window-keybindings close "<Alt>F4"
gsettings set org.mate.Marco.global-keybindings panel-run-dialog "<Alt>F2"
gsettings set org.mate.Marco.global-keybindings show-desktop "<Super>d"
gsettings set org.mate.Marco.global-keybindings switch-windows "<Super>Tab"
gsettings set org.mate.Marco.global-keybindings switch-windows-backward "<Super><Shift>Tab"
gsettings set org.mate.Marco.window-keybindings maximize "<Super>Up"
gsettings set org.mate.Marco.window-keybindings unmaximize "<Super>Down"
gsettings set org.mate.Marco.window-keybindings minimize "<Super>KP_0"

# Some extra for MATE
gsettings set org.mate.Marco.global-keybindings run-command-screenshot "Print"
gsettings set org.mate.Marco.global-keybindings run-command-window-screenshot "<Alt>Print"
gsettings set org.mate.Marco.global-keybindings run-command-terminal "<Control><Alt>t"
