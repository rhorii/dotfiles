arc-theme:
	sudo pacman -S --needed arc-gtk-theme arc-icon-theme elementary-icon-theme

bash:
	sudo pacman -S --needed bash bash-completion
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc ${HOME}/.bashrc

cups:
	sudo pacman -S --needed cups cups-pdf system-config-printer
	sudo systemctl enable org.cups.cupsd.service

docker:
	sudo pacman -S --needed docker docker-compose
	sudo systemctl enable docker.service
	sudo usermod -aG docker rhorii

emacs:
	sudo pacman -S --needed emacs aspell-en
	ln -sfnv ${PWD}/.aspell.conf ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.emacs.d ${HOME}/.emacs.d

exfat:
	sudo pacman -S --needed exfat-utils

firefox:
	sudo pacman -S --needed firefox firefox-i18n-ja

fish:
	sudo pacman -S --needed fish

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
		eog \
		evince \
		gdm \
		gnome-backgrounds \
		gnome-control-center \
		gnome-keyring \
		gnome-screenshot \
		gnome-shell-extensions \
		gnome-system-monitor \
		gnome-terminal \
		gnome-tweaks \
		gvfs-smb \
		libgnome-keyring \
		nautilus \
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

noto-fonts:
	sudo pacman -S --needed noto-fonts noto-fonts-cjk noto-fonts-emoji

python:
	sudo pacman -S --needed python python-pip
	pip install --user --upgrade pip
	pip install --user flake8

ricty:
	yay -S --needed ttf-ricty

ruby:
	yay -S --needed rbenv ruby-build

slack:
	yay -S --needed slack-desktop

systemd-boot:
	yay -S --needed systemd-boot-pacman-hook

systemd-swap:
	sudo pacman -S --needed systemd-swap
	xdg-open 'https://wiki.archlinux.org/index.php/Swap#systemd-swap'
	sudo nano /etc/systemd/swap.conf

	xdg-open 'https://github.com/Nefelim4ag/systemd-swap'
	sudo nano /etc/sysctl.d/99-sysctl.conf

	sudo sysctl -p /etc/sysctl.d/99-sysctl.conf
	sudo systemctl enable systemd-swap.service

yay:
	yay -S --needed yay
