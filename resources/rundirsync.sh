#!/bin/bash
echo off

cd ~/.local/bin

date >> /var/log/dirsync/dirsync.log
./dirsync dirsync.conf >> /var/log/dirsync/dirsync.log
date >> /var/log/dirsync/dirsync.log


