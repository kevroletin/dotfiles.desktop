#!/bin/sh


PACKAGES="xmonad libghc-xmonad-contrib-dev xmobar rxvt-unicode trayer xclip dmenu"
echo "=== Installing $PACKAGES ==="
sudo apt-get install $PACKAGES

echo "=== Configuring desktop session ==="
SESSION_FILE=/usr/share/xsessions/xmonad.desktop
sudo sed -i "s/Exec=xmonad-session/Exec=\/home\/$USER\/.xmonad\/.xsessionrc/g" $SESSION_FILE
sudo sed -i "s/Exec=xmonad/Exec=\/home\/$USER\/.xmonad\/.xsessionrc/g" $SESSION_FILE

while true; do
    read -p "Do you wish to install gui version of emacs? " yn
    case $yn in
        [Yy]* ) sudo apt-get install emacs; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done
