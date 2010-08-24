Angel
=====

`angel` is a daemon that runs and monitors other processes.  It
is similar to djb's `daemontools` or the Ruby project `god`.

It's goals are to keep a set of services running, and to facilitate
the easy configuration and restart of those services.

Motivation
----------

The author is a long-time user of `daemontools` due to its reliability
and simplicity; however, `daemontools` is quirky and follows many
unusual conventions.  

`angel` is an attempt to recreate `daemontools`'s capabilities (though 
not the various bundled utility programs which are still quite useful) 
in a more intuitive and modern unix style.


Functionality
-------------

`angel` is driven by a configuration file that contains a list of
program specifications to run.  Angel assumes every program listed in 
the specification file should be running at all times.

`angel` starts each program, and optionally sets the program's stdout
and stderr to some file(s) which have been opened in append mode; at
this point, the program is said to be "supervised".

If the program dies for any reason, `angel` waits a specified number
of seconds (default, 5), then restarts the program.

The `angel` process itself will respond to a HUP signal by 
re-processing its configuration file, and synchronizing the run
states with the new configuration.  Specifically:

 * If a new program has been added to the file, it is started and
   supervised
 * If a program's specification has changed (command line path,
   stdin/stdout path, delay time, etc) that supervised child
   process will be sent a TERM signal, and as a consequence of
   normal supervision, will be restarted with the updated spec
 * If a program has been removed from the configuration file,
   the corresponding child process will be sent a TERM signal;
   when it dies, supervision of the process will end, and 
   therefore, it will not be restarted

Safety and Reliability
----------------------

Because of `angel`'s role in policing the behavior of other
daemons, it has been written to be very reliable:

 * It is written in Haskell, which boasts a combination of
   strong, static typing and purity-by-default that lends 
   itself to very low bug counts
 * It uses multiple, simple, independent lightweight threads
   with specific roles, ownership, and interfaces
 * It uses STM for mutex-free state synchronization between 
   these threads
 * It falls back to polling behavior to ensure eventual
   synchronization between configuration state and run
   state, just in case odd timing issues should make
   event-triggered changes fail
 * It simply logs errors and keeps running the last good 
   configuration if it runs into problems on configuration
   reloads

Building
--------

 1. Install the haskell-platform 2010 (or somehow, ghc 6.12 + 
    cabal-install)
 2. Run `cabal install` in the project root (this directory)
 3. Either add the ~/.cabal/bin file to your $PATH or copy
    the `angel` executable to /usr/local/bin

Notes:

 * I have not tried building `angel` against ghc 6.10 or earlier

Configuration and Usage Example
-------------------------------

The `angel` executable takes exactly one argument: a path to
an angel configuration file.

Angel configuration files are quasi-INI style configuration files;
here's an example:

    [watch-date]
    exec watch date

    [ls]
    exec ls
    stdout /tmp/ls_log
    stderr /tmp/ls_log
    delay 7

Each program that should be supervised stats with a `program-id` block:

    [watch-date]

Then, a series of corresponding configuration commands follow:

 * `exec` is the exact command line to run (required)
 * `stdout` is a path to a file where the program's standard output 
    should be appended (optional, defaults to /dev/null)
 * `stderr` is a path to a file where the program's standard error
    should be appended (optional, defaults to /dev/null)
 * `delay` is the number of seconds (integer) `angel` should wait
   after the program dies before attempting to start it again
   (optional, defaults to 5)

Assuming the above configuration was in a file called "example.conf",
here's what a shell session might look like:

    jamie@choo:~/random/angel$ angel example.conf 
    [2010/08/24 15:21:22] {main} Angel started
    [2010/08/24 15:21:22] {main} Using config file: example.conf
    [2010/08/24 15:21:22] {process-monitor} Must kill=0, must start=2
    [2010/08/24 15:21:22] {- program: watch-date -} START
    [2010/08/24 15:21:22] {- program: watch-date -} RUNNING
    [2010/08/24 15:21:22] {- program: ls -} START
    [2010/08/24 15:21:22] {- program: ls -} RUNNING
    [2010/08/24 15:21:22] {- program: ls -} ENDED
    [2010/08/24 15:21:22] {- program: ls -} WAITING
    [2010/08/24 15:21:29] {- program: ls -} RESTART
    [2010/08/24 15:21:29] {- program: ls -} START
    [2010/08/24 15:21:29] {- program: ls -} RUNNING
    [2010/08/24 15:21:29] {- program: ls -} ENDED
    [2010/08/24 15:21:29] {- program: ls -} WAITING

.. etc

You can see that when the configuration is parsed, the process-monitor
notices that two programs need to be started.  A supervisor is started
in a lightweight thread for each, and starts logging with the context
`program: <program-id>`.

`watch-date` starts up and runs.  Since `watch` is a long-running process
it just keeps running in the background.

`ls`, meanwhile, runs and immediately ends, of course; then, the WAITING
state is entered until `delay` seconds pass.  Finally, the RESTART event
is triggered and it is started again, ad naseum.

Now, let's see what happens if we modify the config file to look like this:

    #[watch-date]
    #exec watch date
     
    [ls]
    exec ls
    stdout /tmp/ls_log
    stderr /tmp/ls_log
    delay 7

.. and then send HUP to angel.

    [2010/08/24 15:33:59] {config-monitor} HUP caught, reloading config
    [2010/08/24 15:33:59] {process-monitor} Must kill=1, must start=0
    [2010/08/24 15:33:59] {- program: watch-date -} ENDED
    [2010/08/24 15:33:59] {- program: watch-date -} QUIT
    [2010/08/24 15:34:03] {- program: ls -} RESTART
    [2010/08/24 15:34:03] {- program: ls -} START
    [2010/08/24 15:34:03] {- program: ls -} RUNNING
    [2010/08/24 15:34:03] {- program: ls -} ENDED
    [2010/08/24 15:34:03] {- program: ls -} WAITING

As you can see, the config monitor reloaded on HUP, and then the
process monitor marked the watch-date process for killing.  TERM
was sent to the child process, and then the supervisor loop QUIT
because the watch-date program no longer had a config entry.

FAQ
---

**Can I have multiple programs logging to the same file?**

Yes, angel `dup()`s file descriptors and makes effort to safely
allow concurrent writes by child programs; you should DEFINITELY
make sure your child program is doing stdout/stderr writes in 
line-buffered mode so this doesn't result in a complete interleaved
mess in the log file.

**Will angel restart programs for me?**

No; the design is just to send your programs TERM, then `angel` will
restart them.  `angel` tries to work in harmony with traditional
Unix process management conventions.

**How can I take a service down without wiping out its configuration?**

Currently, the only way to achieve this is by employing the "commenting
out" method illustrated above.

Author
------

Jamie Turner <jamie@jamwt.com>

Thanks to Bump Technologies, Inc. (http://bu.mp) for sponsoring some
of the work on angel.
