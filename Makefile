link:
	ln -sfnv ${PWD}/.aspell.conf  ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc       ${HOME}/.bashrc
	ln -sfnv ${PWD}/.emacs.d      ${HOME}/.emacs.d
	ln -sfnv ${PWD}/.gitconfig    ${HOME}/.gitconfig
	touch ${HOME}/.gitconfig.local

arc-theme:
	sudo pacman -S arc-gtk-theme arc-icon-theme elementary-icon-theme

bash:
	sudo pacman -S bash-completion

cups:
	sudo pacman -S cups cups-pdf system-config-printer
	sudo systemctl enable org.cups.cupsd.service

docker:
	sudo pacman -S docker docker-compose
	sudo systemctl enable docker.service
	sudo usermod -aG docker rhorii

emacs:
	sudo pacman -S emacs aspell-en

firefox:
	sudo pacman -S firefox firefox-i18n-ja

fish:
	sudo pacman -S fish

fzf:
	sudo pacman -S fzf

ghq:
	yay -S ghq

gnome:
	sudo pacman -S chrome-gnome-shell \
                 evince \
                 gdm \
                 gnome-backgrounds \
                 gnome-control-center \
                 gnome-keyring \
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
	yay -S ibus-mozc

ipafont:
	sudo pacman -S otf-ipafont

jq:
	yay -S jq

libreoffice:
	sudo pacman -S libreoffice-fresh libreoffice-fresh-ja

noto-fonts:
	sudo pacman -S noto-fonts noto-fonts-cjk noto-fonts-emoji

python:
	sudo pacman -S python python-pip
	pip install --user --upgrade pip
	pip install --user flake8

ricty:
	yay -S ttf-ricty

ruby:
	yay -S rbenv ruby-build

slack:
	yay -S slack-desktop

systemd-boot:
	yay -S systemd-boot-pacman-hook
