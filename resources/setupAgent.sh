#!/bin/bash

sudo mkdir /usr/local/libexec
cd /usr/local/libexec
sudo ln -s ~/.local/bin/dirsync dirsync
sudo ln -s ~/.local/bin/dirsync.conf dirsync.conf
cp resources/kenny.eric.dirsync.plist /Library/LaunchAgents/

