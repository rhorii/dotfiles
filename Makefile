link:
	ln -sfnv ${PWD}/.aspell.conf  ${HOME}/.aspell.conf
	ln -sfnv ${PWD}/.bash_profile ${HOME}/.bash_profile
	ln -sfnv ${PWD}/.bashrc       ${HOME}/.bashrc
	ln -sfnv ${PWD}/.emacs.d      ${HOME}/.emacs.d
	ln -sfnv ${PWD}/.gitconfig    ${HOME}/.gitconfig
	touch ${HOME}/.gitconfig.local
