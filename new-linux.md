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

Terminal:

dconf write /org/gnome/shell/keybindings/toggle-message-tray "['']"
dconf write /org/gnome/shell/keybindings/toggle-overview "['']"
dconf write /org/gnome/Terminal/Legacy/Keybindings/copy "<Super>c"
dconf write /org/gnome/Terminal/Legacy/Keybindings/paste "<Super>v"
dconf write /org/gnome/desktop/interface/enable-hot-corners false
