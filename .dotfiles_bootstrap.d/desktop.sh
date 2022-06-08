#!exec sh

PACKAGES="xmonad libghc-xmonad-contrib-dev xmobar rxvt-unicode trayer xclip dmenu xloadimage redshift feh"

cat <<EOF >> $RESULT_SCRIPT_FILE
"=== Configuring desktop session ==="
SESSION_FILE=/usr/share/xsessions/xmonad.desktop
sudo sed -i "s/Exec=xmonad-session/Exec=\/home\/$USER\/.xmonad\/.xsessionrc/g" $SESSION_FILE
sudo sed -i "s/Exec=xmonad/Exec=\/home\/$USER\/.xmonad\/.xsessionrc/g" $SESSION_FILE
EOF

while true; do
    read -p "Do you wish to install gui version of emacs? " yn
    case $yn in
        [Yy]* ) PACKAGES="$PACKAGES emacs"; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

echo $PACKAGES > $RESULT_PACKAGES_FILE
