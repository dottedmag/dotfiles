# New Linux

## Fedora

```
<enable passwordless sudo>
sudo rpm -e PackageKit-command-not-found
sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf install sway swaybg swaylock swayidle dmenu kitty-terminfo emacs nodejs zsh gitk docker-ce docker-ce-cli docker-buildx-plugin docker-compose-plugin gopass gopass-jsonapi wl-clipboard
sudo usermod -a -G docker $USER
sudo systemctl enable sshd docker
sudo systemctl start sshd docker
chsh -s /usr/bin/zsh
<download chezmoi>
chezmoi init https://github.com/dottedmag/dotfiles
chezmoi update
gopass-jsonapi configure
```

## Debian

## GNOME

For Sculpt (how to disable for other keyboards?):

- dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:ctrl_modifier', 'grp:switch', 'altwin:swap_lalt_lwin', 'dottedmag:sculpt']"

Switch to apps via hotkeys:

- Emacs app 1
- Terminal app 2
- Firefox app3

dconf write /org/gnome/shell/keybindings/switch-to-application-1 "['<Control><Super>e']"
dconf write /org/gnome/shell/keybindings/switch-to-application-2 "['<Control><Super>t']"
dconf write /org/gnome/shell/keybindings/switch-to-application-3 "['<Control><Super>c']"

Remove unneeded hotkeys:

dconf write /org/gnome/shell/keybindings/toggle-message-tray "['']"
dconf write /org/gnome/shell/keybindings/toggle-overview "['']"
dconf write /org/gnome/shell/keybindings/focus-active-notification "['']"
dconf write /org/freedesktop/ibus/panel/emoji/hotkey "['']"
dconf write /org/gnome/mutter/wayland/keybindings/restore-shortcuts "['']"
dconf write /org/gnome/settings-daemon/plugins/media-keys/rotate-video-lock-static "['']"
dconf write /org/gnome/desktop/interface/enable-hot-corners false
dconf write /org/gnome/mutter/overlay-key "''"
dconf write /org/gnome/desktop/wm/keybindings/switch-input-source "['']"
dconf write /org/gnome/desktop/wm/keybindings/switch-input-source-backward "['']"
dconf write /org/gnome/mutter/keybindings/switch-monitor "['']"
dconf write /org/gnome/shell/keybindings/toggle-application-view "['']"

Add needed hotkeys:

dconf write /org/gnome/Terminal/Legacy/Keybindings/copy "<Super>c"
dconf write /org/gnome/Terminal/Legacy/Keybindings/paste "<Super>v"
# Replaces Cmd+A
dconf write /org/gnome/shell/keybindings/toggle-application-view "['<Control><Super>Space']"

Misc:

dconf write /org/gnome/desktop/notifications/show-in-lock-screen false
dconf write /org/gnome/desktop/search-providers/disable-external false
