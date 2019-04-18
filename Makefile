link:
	ln -sfnv ${PWD}/.aspell.conf  ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc       ${HOME}/.bashrc
	ln -sfnv ${PWD}/.emacs.d      ${HOME}/.emacs.d
	ln -sfnv ${PWD}/.gitconfig    ${HOME}/.gitconfig
	touch ${HOME}/.gitconfig.local

bash:
	brew install bash bash-completion@2

docker:
	brew cask install docker

emacs:
	brew install aspell
	brew cask install emacs

firefox:
	brew cask install firefox

fish:
	brew install fish

fzf:
	brew install fzf

ghq:
	brew install ghq

jq:
	brew install jq

python:
	brew install python@2 python
	pip3 install --user --upgrade pip

ricty:
	brew tap sanemat/font
	brew install ricty
	cp -f /usr/local/opt/ricty/share/fonts/Ricty*.ttf ~/Library/Fonts/
	fc-cache -vf

ruby:
	brew install rbenv ruby-build

slack:
	brew cask install slack
