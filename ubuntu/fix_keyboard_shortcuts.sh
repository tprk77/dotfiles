#!/bin/bash

# Fixes the default keyboard shortcuts to work with Emacs,
# etc. Removes annoying defaults. Maybe adds other shortcuts.

# zero all shortcuts
for keybinding in $(gsettings list-keys org.gnome.desktop.wm.keybindings); do
    dconf write /org/gnome/desktop/wm/keybindings/$keybinding "@as []"
done

# add back shortcuts we want
dconf write /org/gnome/desktop/wm/keybindings/close "@as ['<Alt>F4']"
dconf write /org/gnome/desktop/wm/keybindings/panel-run-dialog "@as ['<Alt>F2']"
dconf write /org/gnome/desktop/wm/keybindings/show-desktop "@as ['<Super>d']"
dconf write /org/gnome/desktop/wm/keybindings/switch-applications "@as ['<Super>Tab']"
dconf write /org/gnome/desktop/wm/keybindings/switch-applications-backward "@as ['<Super><Shift>Tab']"
dconf write /org/gnome/desktop/wm/keybindings/maximize "@as ['<Super>Up']"
dconf write /org/gnome/desktop/wm/keybindings/unmaximize "@as ['<Super>Down']"
dconf write /org/gnome/desktop/wm/keybindings/minimize "@as ['<Super>KP_0']"
