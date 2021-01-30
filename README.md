# maildir_commander
maildir commander is a wrapper arond maildir-utils to make it more versatile. `mu server` is already state-less; however, it cannot be used by more than one client at a time. maildir_commander runs one instance of `mu server` in the background and make it an always-on daemon, so mu4e, command-line client and future web client can share the same underlying `mu server` instance through maildir_commander.

## dependency

* [mu](https://www.djcbsoftware.nl/code/mu/)
* erlang OTP 21+
* socat, which is used to re-export the `mu server` compatible interface

## the server

maildir_commander can be used as a dependancy of a larger application or a stand-alone application. In the stand-aline mode, just run the release. It will run `mu server` in the background and communicate with it through a port. All functionalities of `mu server` are exported as erlang APIs via the `mc_mu_api` module.

## the socket

maildir_commander will also listen on a Unix domain socket `$HOME/.mc_server_sock`. The socket once connected, behave exactly like `mu server`. All commands are passed through to the shared `mu server` instance, with the only exception of the `cmd:quit` command, which is locally handled. so:

```
$ socat UNIX-CONNECT:$HOME/.mc_server_sock -
```

behave exactly like `mu server`, except now you can run multiple instances. I have a shell script in `scripts/mc` that can be a drop in repleacement of mu (now only `mc server` is supported, in the future I plan to re-implement all mu client side sub-commands.

## Future works

maildir_commander is meant to be used as the lower layer of more sofisticated MUAs such as a web client, a IMAP client, etc, so they can run along side with the command line client and mu4e.




