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
		chrome-gnome-shell \
		eog \
		evince \
		gdm \
		gnome-backgrounds \
		gnome-control-center \
		gnome-keyring \
		gnome-screenshot \
		gnome-system-monitor \
		gnome-terminal \
		gnome-tweaks \
		gvfs-smb \
		libgnome-keyring \
		nautilus \
		networkmanager \
		xdg-user-dirs
	sudo systemctl enable gdm.service
	sudo systemctl enable NetworkManager.service

	cd /usr/share/git/credential/gnome-keyring;\
	sudo make
	git config --file ${HOME}/.gitconfig.local \
		credential.helper /usr/lib/git-core/git-credential-gnome-keyring

ibus-mozc:
	yay -S --needed ibus-mozc

ipafont:
	sudo pacman -S --needed otf-ipafont

jq:
	yay -S --needed jq

libreoffice:
	sudo pacman -S --needed libreoffice-fresh libreoffice-fresh-ja

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

yay:
	yay -S --needed yay
