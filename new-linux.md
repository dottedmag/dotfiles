# New Linux

## Fedora

```
<enable passwordless sudo>
sudo rpm -e PackageKit-command-not-found
sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf install sway swaybg swaylock swayidle dmenu kitty-terminfo emacs nodejs zsh gitk docker-ce docker-ce-cli docker-buildx-plugin docker-compose-plugin
sudo usermod -a -G docker $USER
sudo systemctl enable sshd docker
sudo systemctl start sshd docker
chsh -s /usr/bin/zsh
<download chezmoi>
chezmoi init https://github.com/dottedmag/dotfiles
chezmoi update
```

## Debian

## GNOME

For Sculpt (how to disable for other keyboards?):

- dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:ctrl_modifier', 'grp:switch', 'altwin:swap_lalt_lwin', 'dottedmag:sculpt']"

Terminal:

- dconf write /org/gnome/terminal/legacy/keybindings/copy "<Super>c"
- dconf paste /org/gnome/terminal/legacy/keybindings/paste "<Super>v"
