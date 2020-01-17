arc-theme:
	sudo pacman -S --needed arc-gtk-theme arc-icon-theme elementary-icon-theme

bash:
	sudo pacman -S --needed bash bash-completion
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc ${HOME}/.bashrc

chromium:
	sudo pacman -S --needed chromium

cups:
	sudo pacman -S --needed cups cups-pdf system-config-printer
	sudo systemctl enable org.cups.cupsd.service

docker:
	sudo pacman -S --needed docker docker-compose
	sudo systemctl enable docker.service
	sudo usermod -aG docker rhorii
	yay -S --needed hadolint-bin

dropbox:
	yay -S --needed dropbox nautilus-dropbox

emacs:
	sudo pacman -S --needed emacs aspell-en ledger
	ln -sfnv ${PWD}/.aspell.conf ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.emacs.d ${HOME}/.emacs.d

eog:
	sudo pacman -S --needed eog

evince:
	sudo pacman -S --needed evince poppler-data

evolution:
	sudo pacman -S --needed evolution evolution-ews

exfat:
	sudo pacman -S --needed exfat-utils

firefox:
	sudo pacman -S --needed firefox firefox-i18n-ja

fzf:
	sudo pacman -S --needed fzf

ghq:
	yay -S --needed ghq

git:
	sudo pacman -S --needed git
	ln -sfnv ${PWD}/.gitconfig ${HOME}/.gitconfig
	touch ${HOME}/.gitconfig.local

gnome:
	sudo pacman -S --needed \
		gdm \
		gnome-backgrounds \
		gnome-calculator \
		gnome-control-center \
		gnome-keyring \
		gnome-screenshot \
		gnome-shell-extensions \
		gnome-system-monitor \
		gnome-terminal \
		gnome-tweaks \
		libgnome-keyring \
		networkmanager \
		xdg-user-dirs

	yay -S --needed gnome-shell-pomodoro

	sudo systemctl enable gdm.service
	sudo systemctl enable NetworkManager.service

	cd /usr/share/git/credential/gnome-keyring;\
	sudo make
	git config --file ${HOME}/.gitconfig.local \
		credential.helper /usr/lib/git-core/git-credential-gnome-keyring

ipafont:
	sudo pacman -S --needed otf-ipafont

jq:
	yay -S --needed jq

libreoffice:
	sudo pacman -S --needed libreoffice-fresh libreoffice-fresh-ja

mozc:
	yay -S --needed ibus-mozc
	@echo 'UNCOMMENT: _emacs_mozc="yes"'
	yay -S --needed --editmenu emacs-mozc

nautilus:
	sudo pacman -S --needed nautilus file-roller gvfs-smb sushi

noto-fonts:
	sudo pacman -S --needed noto-fonts noto-fonts-cjk noto-fonts-emoji

pandoc:
	sudo pacman -S --needed pandoc

php:
	sudo pacman -S --needed php

python:
	sudo pacman -S --needed python python-pip
	pip install --user --upgrade pip
	pip install --user flake8

ricty:
	yay -S --needed ttf-ricty

ripgrep:
	sudo pacman -S --needed ripgrep

ruby:
	yay -S --needed rbenv ruby-build

slack:
	yay -S --needed slack-desktop

sshpass:
	sudo pacman -S --needed sshpass

systemd-boot:
	yay -S --needed systemd-boot-pacman-hook

systemd-swap:
	sudo pacman -S --needed systemd-swap
	xdg-open 'https://wiki.archlinux.org/index.php/Swap#systemd-swap'
	sudo nano /etc/systemd/swap.conf.d/99-swap.conf

	xdg-open 'https://github.com/Nefelim4ag/systemd-swap'
	sudo nano /etc/sysctl.d/99-sysctl.conf

	sudo sysctl -p /etc/sysctl.d/99-sysctl.conf
	sudo systemctl enable systemd-swap.service

vagrant:
	sudo pacman -S --needed vagrant

virtualbox:
	sudo pacman -S --needed \
		virtualbox \
		virtualbox-guest-iso \
		virtualbox-host-modules-arch
	sudo gpasswd -a ${USER} vboxusers

yay:
	yay -S --needed yay

zeal:
	sudo pacman -S --needed zeal qt5-styleplugins qt5ct
	sudo ln -sfnv ${PWD}/etc/environment /etc/environment
	mkdir -p ${HOME}/.config/Zeal
	ln -sfnv ${PWD}/.config/Zeal/Zeal.conf ${HOME}/.config/Zeal/Zeal.conf
