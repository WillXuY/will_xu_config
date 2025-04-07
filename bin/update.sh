#!/bin/bash

foot --title "Flatpak Update" -e bash -c "flatpak update -y; echo 'Flatpak 更新完成'; exec bash" &

sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
