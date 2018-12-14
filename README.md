# dirsync

Dirsync is a simple little tool to back up files and directories from the local
file system to another location in the file system (presumably a mounted
network share.)  

It only exists because we don't have time machine backups for our macbooks at
work and I wanted to have some kind of minimal backup until a better solution is
in place.  This is the product of several months worth of lost files.

Dirsync accepts a json-encoded configuration file on the command line.  The json is a
list of source and destination specifications, like this:

```json
  [
    {   
        "source": "/users/me/documents"
      , "destination": "/Volumes/mount_point/documents"
      , "ignore": ["personal", "image.png", ".*.swp"]
    }
    ,
    {  
        "source": "/users/me/sripts"
      , "destination": "/Volumes/mount_point/scripts"
      , "ignore": [".git"]
    }
  ]
```

The "ignore" list now allows wildcards like you would use with bash.


## Building and Installing

Install the stack tool

```
curl -sSL https://get.haskellstack.org/ | sh

```
Now you're ready to build and install.  Just do...

```
stack build
```

```
stack install
```

The binary will be installed in ~/.local/bin


## Running

```bash 
dirsync path/to/dirsync.conf
```

There is an example run script in the resources directory.  To use it, you would
want to modify it for your user id, etc.  

This tool is most useful if you set it up to run periodically using cron.

Right now my `crontab -l` reports this (runs every 20 minutes):

```
*/20 * * * * /Users/<myuserid>/scripts/rundirsync.sh
```

## For running as daemon on Mac OS X

I have been unable to get this to work as a LaunchAgent, so I'm running this
as a cron job for now.  But below are some notes I grabbed about setting
up a LaunchAgent.

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
