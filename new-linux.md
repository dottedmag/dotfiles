# New Linux

## Configuration (GNOME)

For Sculpt (how to disable for other keyboards?):

- dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:ctrl_modifier', 'grp:switch', 'altwin:swap_lalt_lwin', 'dottedmag:sculpt']"

Terminal:

- dconf write /org/gnome/terminal/legacy/keybindings/copy "<Super>c"
- dconf paste /org/gnome/terminal/legacy/keybindings/paste "<Super>v"

