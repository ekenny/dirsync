#!/bin/bash
echo off

pushd ~/.local/bin

date >> /var/log/dirsync/dirsync.log
dirsync >> /var/log/dirsync/dirsync.log
date >> /var/log/dirsync/dirsync.log

popd


