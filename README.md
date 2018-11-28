# dirsync




## For running as daemon on Mac OS X

 For a complete listing of the keys, see the launchd.plist manual page.

https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html#//apple_ref/doc/uid/TP40001762-104142

The appropriate location for executables that you launch from your job is
/usr/local/libexec

You indicate whether it describes a daemon or agent by the directory you place
it in. Property list files describing daemons are installed in
/Library/LaunchDaemons, and those describing agents are installed in
/Library/LaunchAgents or in the LaunchAgents subdirectory of an individual
user’s Library directory.

### Debugging launchd Jobs

There are some options that are useful for debugging your launchd job.

The following example enables core dumps, sets standard out and error to go to a
log file, and instructs launchd to temporarily increase the debug level of its
logging while acting on behalf of your job (remember to adjust your syslog.conf
accordingly):
  
