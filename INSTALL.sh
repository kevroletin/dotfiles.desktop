sudo apt install ncal \
	direnv \
	ripgrep \
	pip \
	pipx \
	python3-full \
	pandoc \
	upx \
  fd-find

sudo apt install calibre \
	transmission-gtk \
	vlc

curl https://github.com/mikefarah/yq/releases/download/v4.44.1/yq_linux_amd64 -o ~/bin/yq && chmod +x $_

# notifications
sudo apt install dunst scrot
mkdir ~/Share

sudo usermod -a -G input behemoth
sudo cp /mnt/usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop

# deal with udev for xremap and bright
sudo chmod a+rw /dev/uinput # this is until reboot
sudo usermod -a -G input behemoth
sudo sh -c 'echo KERNEL=="uinput", GROUP="input", MODE="0660", OPTIONS+="static_node=uinput" >> /lib/udev/rules.d/udev.rules'
sudo sh -c 'echo ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"' >> /lib/udev/rules.d/backlight.rules
sudo sh -c 'echo ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"' >> /lib/udev/rules.d/backlight.rules

# install numen
sudo apt install -y golang-go scdoc libxkbcommon-dev

mkdir -p ~/Scratch/desktop
git clone https://git.sr.ht/~geb/numen ~/Scratch/desktop/numen
pushd ~/Scratch/desktop/numen
  ./get-vosk.sh && sudo ./get-vosk.sh install
  ./get-model.sh && sudo ./get-model.sh install
  ./get-dotool.sh && sudo ./get-dotool.sh install
  ./build.sh && sudo ./build.sh install
popd

#sudo chmod a+rw /sys/class/backlight/intel_backlight/brightness

# install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Office tools
# apt install slack installs something else
sudo snap install slack

# Install microsoft-edge
# https://www.omgubuntu.co.uk/2021/01/how-to-install-edge-on-ubuntu-linux

curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/edge stable main" > /etc/apt/sources.list.d/microsoft-edge-dev.list'
sudo rm microsoft.gpg

sudo apt update && sudo apt install microsoft-edge-stable

xserver-xorg-input-synaptics
sudo apt install xserver-xorg-input-synaptics-hwe-18.04

# Obsidian
sudo apt install libfuse2

# git credentials manager
# https://github.com/git-ecosystem/git-credential-manager/blob/release/docs/install.md
pushd ~Downloads
wget https://github.com/git-ecosystem/git-credential-manager/releases/download/v2.5.0/gcm-linux_amd64.2.5.0.deb
sudo dpkg -i ~/Downloads/gcm-linux_amd64.2.5.0.deb
git config --global credential.credentialStore plaintext
git config --global credential.interactive true
popd

# kubectl
pushd ~/bin
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl" && chmod +x kubectl
curl https://raw.githubusercontent.com/blendle/kns/master/bin/kns -o ~/bin/kns && chmod +x $_
curl https://raw.githubusercontent.com/blendle/kns/master/bin/ktx -o ~/bin/ktx && chmod +x $_

# krew
(
    set -x; cd "$(mktemp -d)" &&
        OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
        ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
        KREW="krew-${OS}_${ARCH}" &&
        curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
        tar zxvf "${KREW}.tar.gz" &&
        ./"${KREW}" install krew
)

kubectl krew install stern

popd

# aws

pushd /tmp
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install
popd

# Transfer .kube and .aws directories to a new machine

# configure git
# https://github.com/git-ecosystem/git-credential-manager/releases/tag/v2.5.0
pushd ~/Downloads
wget https://github.com/git-ecosystem/git-credential-manager/releases/download/v2.5.0/gcm-linux_amd64.2.5.0.deb
sudo dpkg -i gcm-linux_amd64.2.5.0.deb
popd
cd
git-credential-manager configure

# for work
sudo apt install libssl-dev

# adjust brightness
sudo usermod -a -G video behemoth

# desktop notifications
sudo install dante

sudo install chromium-browser
# Fix chinese in Chromium
sudo apt-get install fonts-arphic-ukai fonts-arphic-uming fonts-ipafont-mincho fonts-ipafont-gothic fonts-unfonts-core

# Docker
# https://docs.docker.com/engine/install/ubuntu/
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
    sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

sudo groupadd docker
sudo usermod -aG docker $USER

# https://www.omgubuntu.co.uk/how-to-install-flatpak-on-ubuntu
sudo apt install flatpak
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

#
flatpak install flathub net.mkiol.SpeechNote

# install wireguard
sudo apt install wireguard wireguard-tools
sudo cp /mnt/home/behemoth/Scratch/python/algo/configs/*/wireguard/vas_phone_redmi.conf /etc/wireguard/wg0.conf

# VSCode
sudo snap install --classic code
cp -r /mnt/home/behemoth/.config/Code /home/behemoth/.config/Code

# Fonts
# https://askubuntu.com/questions/193072/how-to-use-the-adobe-source-code-pro-font
mkdir -p /tmp/adodefont
pushd /tmp/adodefont
wget -q --show-progress -O source-code-pro.zip https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
unzip -q source-code-pro.zip -d source-code-pro
fontpath="${XDG_DATA_HOME:-$HOME/.local/share}"/fonts
mkdir -p $fontpath
cp -v source-code-pro/*/OTF/*.otf $fontpath
fc-cache -f
rm -rf source-code-pro{,.zip}

popd

# magit
cargo install git-delta

# Synchthing
# https://apt.syncthing.net/
