bash:
	brew install bash bash-completion@2
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc ${HOME}/.bashrc

docker:
	brew cask install docker

dropbox:
	brew cask install dropbox

emacs:
	brew install aspell
	brew cask install emacs
	ln -sfnv ${PWD}/.aspell.conf ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.emacs.d ${HOME}/.emacs.d

firefox:
	brew cask install firefox

fzf:
	brew install fzf

ghq:
	brew install ghq

git:
	brew install git
	ln -sfnv ${PWD}/.gitconfig ${HOME}/.gitconfig
	ln -sfnv ${PWD}/.gitignore_global ${HOME}/.gitignore_global
	touch ${HOME}/.gitconfig_local

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
