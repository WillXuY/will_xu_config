#!/bin/bash

foot --title "Apt Update" -e bash -c "sudo apt update -y; sudo apt upgrade -y; sudo apt autoremove -y; echo 'apt finished'; exec bash" &

flatpak update -y
