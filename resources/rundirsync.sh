#!/bin/bash

INSTALL_DIR=$HOME/.local/bin
REMOTE_MOUNT_POINT=/Volumes/AUETK
USER_ID=auetk
LOG_FILE=/var/log/dirsync/dirsync.log
CONFIG_FILE=dirsync.conf
RUN_ID=$(uuidgen)


cd "$INSTALL_DIR" 

echo "Run ($RUN_ID) started at $(date)" >> "$LOG_FILE"

# If the remote SMB share hasn't been mounted yet, mount it.
#
# This is currently broken because this needs to be run as root
# and this is intended for unattended execution.  So I have to 
# figure that out.  Meantime, I just make sure my network share
# is mounted at the start of every day.  It's interesting that
# the mount goes away when the machine sleeps.

#if [[ ! -d "$REMOTE_MOUNT_POINT" ]]; then
#  echo "$REMOTE_MOUNT_POINT does not exist. Mounting network share" >> "$LOG_FILE"
#  mkdir "$dREMOTE_MOUNT_POINT"
#  mount_smbfs //$USER_ID@corporate.unificompanies.com/users/Cincinnati/$USER_ID $REMOTE_MOUNT_POINT
#fi

./dirsync "$CONFIG_FILE" >> "$LOG_FILE"

echo "$RUN_ID completed at $(date)" >> "$LOG_FILE"


