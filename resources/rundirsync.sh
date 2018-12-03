#!/bin/bash

echo off

INSTALL_DIR=~./local/bin
REMOTE_MOUNT_POINT=/Volumes/AUETK
USER_ID=auetk
LOG_FILE=/var/log/dirsync/dirsync.log
CONFIG_FILE=dirsync.conf

cd "$INSTALL_DIR" 

date >> "$LOG_FILE"

# if the remote SMB share hasn't been mounted yet, mount it.
if [ ! -d "$REMOTE_MOUNT_POINT"]; then
  echo "$REMOTE_MOUNT_POINT does not exist. Mounting network share" >> "$LOG_FILE"
  mkdir "$dREMOTE_MOUNT_POINT"
  mount_smbfs //$USER_ID@corporate.unificompanies.com/users/Cincinnati/$USER_ID $REMOTE_MOUNT_POINT
fi

./dirsync "$CONFIG_FILE" >> "$LOG_FILE"

date >> "$LOG_FILE"


