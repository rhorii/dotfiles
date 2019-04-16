FROM archlinux/base:latest

ARG USERNAME=rhorii

COPY ./etc /etc
RUN trust extract-compat

RUN pacman -Syu --noconfirm
RUN pacman -S base base-devel --noconfirm

RUN useradd -m -G wheel -s /bin/bash ${USERNAME}
RUN echo "${USERNAME}:" | chpasswd -e
RUN echo '%wheel ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers
RUN echo 'Defaults env_keep += "http_proxy HTTP_PROXY"' >> /etc/sudoers
RUN echo 'Defaults env_keep += "https_proxy HTTPS_PROXY"' >> /etc/sudoers
RUN echo 'Defaults env_keep += "ftp_proxy FTP_PROXY"' >> /etc/sudoers
RUN echo 'Defaults env_keep += "no_proxy NO_PROXY"' >> /etc/sudoers

RUN pacman -S git --noconfirm

USER ${USERNAME}
RUN mkdir -p /home/${USERNAME}/src/github.com/aur \
  && cd /home/${USERNAME}/src/github.com/aur \
  && git clone https://aur.archlinux.org/yay.git \
  && cd /home/${USERNAME}/src/github.com/aur/yay \
  && makepkg -si --noconfirm
