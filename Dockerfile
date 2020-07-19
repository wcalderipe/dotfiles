FROM ubuntu:20.04

RUN apt-get update --quiet
RUN apt-get install sudo git ansible --quiet --yes

RUN useradd --create-home --shell /bin/bash tester
RUN usermod --append --groups sudo tester
RUN echo "tester   ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers

RUN mkdir --parents /home/tester/dev
ADD . /home/tester/dev/dotfiles
RUN chown --recursive tester:tester /home/tester

USER tester
ENV USER tester
ENV HOME /home/tester
ENV DOTFILES_DIR /home/tester/dev/dotfiles
WORKDIR /home/tester/dev/dotfiles


CMD ["/bin/bash"]
