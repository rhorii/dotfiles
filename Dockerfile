FROM archlinux/base:latest

ARG USERNAME=rhorii

COPY ./etc /etc
RUN trust extract-compat

RUN pacman -Syu --noconfirm
RUN pacman -S base base-devel --noconfirm

RUN useradd -m -G wheel -s /bin/bash ${USERNAME}
RUN echo "${USERNAME}:" | chpasswd -e
RUN echo '%wheel ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

RUN pacman -S git go --noconfirm

USER ${USERNAME}
RUN mkdir -p /home/${USERNAME}/src/github.com/aur \
  && cd /home/${USERNAME}/src/github.com/aur \
  && git clone https://aur.archlinux.org/yay.git \
  && cd /home/${USERNAME}/src/github.com/aur/yay \
  && makepkg -si --noconfirm
RUN mkdir -p /home/${USERNAME}/src/github.com/rhorii \
  && cd /home/${USERNAME}/src/github.com/rhorii \
  && git clone https://github.com/rhorii/dotfiles.git
